/*
 *  RODDC/src/RODBC.c 
 *         M. Lapsley Copyright (C) 1999-2002
 *         B. D. Ripley  Copyright (C) 2002-2017
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available at
 *  http://www.r-project.org/Licenses/
 */

/* RODBC low level interface
 *
 */
#include "config.h"
#include <stdio.h>
#include <sys/types.h>
#include <fcntl.h>
#include <stdlib.h>
#include <stddef.h>
#include <math.h>

#ifdef WIN32
# include <windows.h>
# undef ERROR
/* enough of the internals of graphapp objects to allow us to find the
   handle of the RGui main window */
typedef struct objinfo {
	int	kind, refcount;
	HANDLE	handle;
} *window;
__declspec(dllimport) window RConsole;
#else
# include <unistd.h>
#endif

#include <string.h>
#include <limits.h> /* for INT_MAX */

#define MAX_CHANNELS 1000
#include <sql.h>
#include <sqlext.h>

#include <R.h>
#include <Rdefines.h>

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("RODBC", String)
#define gettext_noop(String) String
#else
#define _(String) (String)
#define gettext_noop(String) String
#endif

#define my_min(a,b) ((a < b)?a:b)

#define COLMAX 256
#ifndef SQL_NO_DATA
# define SQL_NO_DATA_FOUND /* for iODBC */
#endif
#define NCOLS thisHandle->nColumns /* save some column space for typing*/
#define NROWS thisHandle->nRows

/* For 64-bit ODBC, Microsoft did some redefining, see
   http://msdn.microsoft.com/library/default.asp?url=/library/en-us/odbc/htm/dasdkodbcoverview_64bit.asp
   Some people think this corresponded to increasing the version to 3.52,
   but none of MinGW, unixODBC or iodbc seem to have done so.

   Given that, how do we know what these mean?

   MinGW[-w64]: if _WIN64 is defined, they are 64-bit, otherwise (unsigned) long.

   unixODBC: if SIZEOF_LONG == 8 && BUILD_REAL_64_BIT_MODE they are
   64-bit.  In applications, SIZEOF_LONG == 8 is determined by
   if defined(__alpha) || defined(__sparcv9) || defined(__LP64__)
   We have no way of knowing if BUILD_REAL_64_BIT_MODE was defined,
   but Debian which does define also modifies the headers.

   iobdc: if _WIN64 is defined, they are 64-bit
   Otherwise, they are (unsigned) long.
 */

#ifndef HAVE_SQLLEN
#define SQLLEN SQLINTEGER
#endif

#ifndef HAVE_SQLULEN
#define SQLULEN SQLUINTEGER
#endif


/* Note that currently we will allocate large buffers for long char
   types whatever rows_at_time is. */
#define MAX_ROWS_FETCH	1024

typedef struct cols {
    SQLCHAR	ColName[256];
    SQLSMALLINT	NameLength;
    SQLSMALLINT	DataType;
    SQLULEN	ColSize;
    SQLSMALLINT	DecimalDigits;
    SQLSMALLINT	Nullable;
    char	*pData;
    int		datalen;
    SQLDOUBLE	RData [MAX_ROWS_FETCH];
    SQLREAL	R4Data[MAX_ROWS_FETCH];
    SQLINTEGER	IData [MAX_ROWS_FETCH];
    SQLSMALLINT	I2Data[MAX_ROWS_FETCH];
    SQLLEN	IndPtr[MAX_ROWS_FETCH];
} COLUMNS;

typedef struct mess {
    SQLCHAR	*message;
    struct mess	*next;
} SQLMSG;

typedef struct rodbcHandle {
    SQLHDBC	hDbc;         /* connection handle */
    SQLHSTMT	hStmt;        /* statement handle */
    SQLLEN	nRows;        /* number of rows and columns in result set */
    SQLSMALLINT	nColumns;
    int		channel;      /* as stored on the R-level object */
    int         id;           /* ditto */
    int         useNRows;     /* value of believeNRows */
    /* entries used to bind data for result sets and updates */
    COLUMNS	*ColData;	/* this will be allocated as an array */
    int		nAllocated;     /* how many cols were allocated */
    SQLUINTEGER	rowsFetched;	/* use to indicate the number of rows fetched */
    SQLUINTEGER	rowArraySize;	/* use to indicate the number of rows we expect back */
    SQLUINTEGER	rowsUsed;	/* for when we fetch more than we need */
    SQLMSG	*msglist;	/* root of linked list of messages */
    SEXP        extPtr;		/* address of external pointer for this 
				   channel, so we can clear it */
} RODBCHandle, *pRODBCHandle;

static unsigned int nChannels = 0; /* number of channels opened in session */
static pRODBCHandle opened_handles[MAX_CHANNELS+1];

static SQLHENV hEnv=NULL;

/* forward declarations */
static void geterr(pRODBCHandle thisHandle);
static void errorFree(SQLMSG *node);
static void errlistAppend(pRODBCHandle thisHandle, const char *string);
static int cachenbind(pRODBCHandle thisHandle, int nRows);


/* Error messages */

static char 
err_SQLAllocStmt[]=gettext_noop("[RODBC] ERROR: Could not SQLAllocStmt");

/* called at first connection or first listing of data sources */
static void odbcInit(void)
{
    SQLRETURN retval;

    if(!hEnv) {
	retval = SQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, &hEnv);
	if(retval == SQL_SUCCESS || retval == SQL_SUCCESS_WITH_INFO) {
	    SQLSetEnvAttr(hEnv, SQL_ATTR_ODBC_VERSION,
			  (SQLPOINTER) SQL_OV_ODBC3, SQL_IS_INTEGER);
	} else
	    error(_("[RODBC] ERROR: Could not SQLAllocEnv"));
    }
}

/* called from .onUnload */
SEXP RODBCTerm(void)
{
    if(!hEnv) SQLFreeHandle(SQL_HANDLE_ENV, hEnv);
    return R_NilValue;
}


/* called before SQL queries (indirect and direct) */
static void clearresults(pRODBCHandle thisHandle)
{
    if(thisHandle->hStmt) {
	(void)SQLFreeStmt(thisHandle->hStmt, SQL_CLOSE);
	(void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);
	thisHandle->hStmt = NULL;
    }
    errorFree(thisHandle->msglist);
    thisHandle->msglist=NULL;
}

SEXP RODBCclearresults(SEXP chan)
{
    clearresults(R_ExternalPtrAddr(chan));
    return R_NilValue;
}

/**********************************************
 *	CONNECT
 *		returns channel no in stat
 *		or -1 on error
 *		saves connect data in handles[channel]
 *
 *	***************************************/
static void chanFinalizer(SEXP ptr);

#define buf1_len 8096
SEXP RODBCDriverConnect(SEXP connection, SEXP id, SEXP useNRows, SEXP ReadOnly)
{
    SEXP ans;
    SQLSMALLINT tmp1;
    SQLRETURN retval;
    SQLCHAR buf1[buf1_len];
    pRODBCHandle thisHandle;

    PROTECT(ans = allocVector(INTSXP, 1));
    INTEGER(ans)[0] = -1;
    if(!isString(connection)) {
	warning(_("[RODBC] ERROR:invalid connection argument"));
	UNPROTECT(1);
	return ans;
    }
    thisHandle = Calloc(1, RODBCHandle);
    ++nChannels;

    odbcInit();
    retval = SQLAllocHandle(SQL_HANDLE_DBC, hEnv, &thisHandle->hDbc);
    if(retval == SQL_SUCCESS || retval == SQL_SUCCESS_WITH_INFO) {
	if(asLogical(ReadOnly))
	    SQLSetConnectAttr(thisHandle->hDbc, SQL_ATTR_ACCESS_MODE, 
			      (SQLPOINTER) SQL_MODE_READ_ONLY, 0);
	retval =
	    SQLDriverConnect(thisHandle->hDbc,
#ifdef WIN32
			     RConsole ? RConsole->handle : NULL,
#else
			     NULL,
#endif
			     /* This loses the const, but although the
				declaration is not (const SQLCHAR *),
				it should be. */
			     (SQLCHAR *) translateChar(STRING_ELT(connection, 0)),
			     SQL_NTS,
			     (SQLCHAR *) buf1,
			     (SQLSMALLINT) buf1_len,
			     &tmp1,
#ifdef WIN32
			     RConsole ? SQL_DRIVER_COMPLETE : SQL_DRIVER_NOPROMPT
#else
			     SQL_DRIVER_NOPROMPT
#endif
		);
	if(retval == SQL_SUCCESS || retval == SQL_SUCCESS_WITH_INFO) {
	    SEXP constr, ptr;

	    ptr = R_MakeExternalPtr(thisHandle, install("RODBC_channel"),
				    R_NilValue);
	    PROTECT(ptr);
	    R_RegisterCFinalizerEx(ptr, chanFinalizer, TRUE);
	    PROTECT(constr = allocVector(STRSXP, 1));
	    SET_STRING_ELT(constr, 0, mkChar((char *)buf1));
	    thisHandle->nColumns = -1;
	    thisHandle->channel = nChannels;
	    thisHandle->useNRows = asInteger(useNRows);
	    thisHandle->id = asInteger(id);
	    thisHandle->extPtr = ptr;
	    /* return the channel no */
	    INTEGER(ans)[0] = nChannels;
	    /* and the connection string as an attribute */
	    setAttrib(ans, install("connection.string"), constr);
	    setAttrib(ans, install("handle_ptr"), ptr);
	    /* Rprintf("opening %d (%p, %p)\n", nChannels,
	       ptr, thisHandle); */
	    if(nChannels <= MAX_CHANNELS) opened_handles[nChannels] = thisHandle;
	    UNPROTECT(3);
	    return ans;
	} else {
	    if (retval == SQL_ERROR) {
		SQLCHAR state[6], msg[1000];
		SQLSMALLINT buffsize=1000, msglen, i=1;
		SQLINTEGER code;
		while(1) {
		    retval =  SQLGetDiagRec(SQL_HANDLE_DBC,
					    thisHandle->hDbc, i++,
					    state, &code, msg, buffsize,
					    &msglen);
		    if(retval == SQL_NO_DATA_FOUND) break;
		    warning(_("[RODBC] ERROR: state %s, code %d, message %s"),
			    state, code, msg);
		}
	    } else warning(_("[RODBC] ERROR: Could not SQLDriverConnect"));
	    (void)SQLFreeHandle(SQL_HANDLE_DBC, thisHandle->hDbc);
	}
    } else {
	warning(_("[RODBC] ERROR: Could not SQLAllocConnect"));
    }
    UNPROTECT(1);
    return ans;
}

/**********************************************************
 *
 *	QUERY
 *		run the query on channel pointed to by chan
 *		cache rols and cols returned in handles[channel]
 *		cache col descriptor data in handles[channel].ColData
 *		return -1 in stat on error or 1
 * *****************************************************/
SEXP RODBCQuery(SEXP chan, SEXP query, SEXP sRows)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLRETURN res;
    int nRows = asInteger(sRows), stat = 1;
    const char *cquery;

    if(nRows == NA_INTEGER || nRows < 1) nRows = 1;

    clearresults(thisHandle);
    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc,
			 &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
    } else {
	cquery = translateChar(STRING_ELT(query, 0));
	/* another case of a missing 'const' */
	res = SQLExecDirect(thisHandle->hStmt,
			    (SQLCHAR *) translateChar(STRING_ELT(query, 0)),
			    SQL_NTS);
	if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	    char *message = Calloc(strlen(cquery)+50, char);
	    sprintf(message,
		    "[RODBC] ERROR: Could not SQLExecDirect '%s'", cquery);
	    geterr(thisHandle);
	    errlistAppend(thisHandle, message);
	    (void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);
	    thisHandle->hStmt = NULL;
	    stat = -1;
	} else
	    stat = cachenbind(thisHandle, nRows);
    }
    return ScalarInteger(stat);
}

#define success_or_failure(message) \
	if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {\
	    geterr(thisHandle);\
	    (void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);\
	    thisHandle->hStmt = NULL;\
	    errlistAppend(thisHandle, message);\
	    stat = -1;\
	} else stat = cachenbind(thisHandle, 1);

/****************************************************
 *
 * get primary key
 *
 * *************************************************/
SEXP RODBCPrimaryKeys(SEXP chan, SEXP table, SEXP cat, SEXP schem)
{
    int stat = 1;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLRETURN res;
    const char *catalog = NULL, *schema = NULL;
    SQLSMALLINT len1 = 0, len2 = 0;

    clearresults(thisHandle);
    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
    } else {
	if(TYPEOF(cat) == STRSXP && LENGTH(cat)) {
	    catalog = translateChar(STRING_ELT(cat, 0));
	    len1 = (SQLSMALLINT) strlen(catalog);
	}
	if(TYPEOF(schem) == STRSXP && LENGTH(schem)) {
	    schema = translateChar(STRING_ELT(schem, 0));
	    len2 = (SQLSMALLINT) strlen(schema);
	}

	/* another case of a missing 'const' */
	res = SQLPrimaryKeys( thisHandle->hStmt,
			      (SQLCHAR *) catalog, len1,
			      (SQLCHAR *) schema, len2,
			      (SQLCHAR *) translateChar(STRING_ELT(table, 0)),
			      SQL_NTS);
	success_or_failure(_("[RODBC] ERROR: Failure in SQLPrimary keys"));
    }
    return ScalarInteger(stat);
}


/********************************************
 *
 *	Get column data
 *
 *	********************************/
SEXP RODBCColumns(SEXP chan, SEXP table, SEXP cat, SEXP schem, SEXP sLiteral)
{
    int stat = 1;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLRETURN res;
    const char *catalog = NULL, *schema = NULL;
    int literal;
    SQLSMALLINT len1 = 0, len2 = 0;

    clearresults(thisHandle);
    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
    } else {
	if(TYPEOF(cat) == STRSXP && LENGTH(cat)) {
	    catalog = translateChar(STRING_ELT(cat, 0));
	    len1 = (SQLSMALLINT) strlen(catalog);
	}
	if(TYPEOF(schem) == STRSXP && LENGTH(schem)) {
	    schema = translateChar(STRING_ELT(schem, 0));
	    len2 = (SQLSMALLINT) strlen(schema);
	}
	literal = asLogical(sLiteral);
	if(literal == NA_LOGICAL) literal = 0;
	if(literal)
	    res = SQLSetStmtAttr(thisHandle->hStmt, SQL_ATTR_METADATA_ID,
				 (SQLPOINTER) SQL_TRUE, SQL_IS_UINTEGER);

	/* another case of a missing 'const' */
	res = SQLColumns( thisHandle->hStmt,
			  (SQLCHAR *) catalog, len1,
			  (SQLCHAR *) schema, len2,
			  (SQLCHAR *) translateChar(STRING_ELT(table, 0)),
			  SQL_NTS, NULL, 0);
	success_or_failure(_("[RODBC] ERROR: Failure in SQLColumns"));
    }
    return ScalarInteger(stat);
}


SEXP RODBCSpecialColumns(SEXP chan, SEXP table, SEXP cat, SEXP schem)
{
    int stat = 1;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLRETURN res;
    const char *catalog = NULL, *schema = NULL;
    SQLSMALLINT len1 = 0, len2 = 0;

    clearresults(thisHandle);
    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
    } else {
	if(TYPEOF(cat) == STRSXP && LENGTH(cat)) {
	    catalog = translateChar(STRING_ELT(cat, 0));
	    len1 = (SQLSMALLINT) strlen(catalog);
	}
	if(TYPEOF(schem) == STRSXP && LENGTH(schem)) {
	    schema = translateChar(STRING_ELT(schem, 0));
	    len2 = (SQLSMALLINT) strlen(schema);
	}

	/* another case of a missing 'const' */
	res = SQLSpecialColumns( thisHandle->hStmt, SQL_BEST_ROWID,
				 (SQLCHAR *) catalog, len1,
				 (SQLCHAR *) schema, len2,
				 (SQLCHAR *) translateChar(STRING_ELT(table, 0)),
				 SQL_NTS,
				 SQL_SCOPE_TRANSACTION, SQL_NULLABLE);
	success_or_failure(_("[RODBC] ERROR: Failure in SQLSpecialColumns"));
    }
    return ScalarInteger(stat);
}

/*****************************************************
 *
 *    get Table data
 *
 * ***************************************/

SEXP RODBCTables(SEXP chan, SEXP cat, SEXP schem, SEXP name, SEXP type,
		 SEXP sLiteral)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLRETURN res;
    const char *catalog = NULL, *schema = NULL, *tName = NULL, *tType = NULL;
    int stat = 1, literal;
    SQLSMALLINT len1 = 0, len2 = 0, len3 = 0, len4 = 0;

    clearresults(thisHandle);

    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
    } else {
	if(TYPEOF(cat) == STRSXP && LENGTH(cat)) {
	    catalog = translateChar(STRING_ELT(cat, 0));
	    len1 = (SQLSMALLINT) strlen(catalog);
	}
	if(TYPEOF(schem) == STRSXP && LENGTH(schem)) {
	    schema = translateChar(STRING_ELT(schem, 0));
	    len2 = (SQLSMALLINT) strlen(schema);
	}
	if(TYPEOF(name) == STRSXP && LENGTH(name)) {
	    tName = translateChar(STRING_ELT(name, 0));
	    len3 = (SQLSMALLINT) strlen(tName);
	}
	if(TYPEOF(type) == STRSXP && LENGTH(type)) {
	    tType = translateChar(STRING_ELT(type, 0));
	    len4 = (SQLSMALLINT) strlen(tType);
	}

	literal = asLogical(sLiteral);
	if(literal == NA_LOGICAL) literal = 0;
	if(literal)
	    res = SQLSetStmtAttr(thisHandle->hStmt, SQL_ATTR_METADATA_ID,
				 (SQLPOINTER) SQL_TRUE, SQL_IS_UINTEGER);

	res = SQLTables(thisHandle->hStmt,
			(SQLCHAR *) catalog, len1,
			(SQLCHAR *) schema, len2,
			(SQLCHAR *) tName, len3,
			(SQLCHAR *) tType, len4);
	success_or_failure(_("[RODBC] ERROR: SQLTables failed"));
    }
    return ScalarInteger(stat);
}

/*****************************************************
 *
 *    get Type Info
 *
 * ***************************************/

SEXP RODBCTypeInfo(SEXP chan,  SEXP ptype)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    short type;
    SQLRETURN res;
    int stat = TRUE;

    clearresults(thisHandle);
    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = FALSE;
    } else {
	switch(asInteger(ptype)){
	case 0: type = SQL_ALL_TYPES; break; /* all */
	case 1: type = SQL_CHAR; break;
	case 2: type = SQL_VARCHAR; break;
	case 3: type = SQL_REAL; break;
	case 4: type = SQL_DOUBLE; break;
	case 5: type = SQL_INTEGER; break;
	case 6: type = SQL_SMALLINT; break;
	case 7: type = SQL_TYPE_TIMESTAMP; break;
	case 8: type = SQL_FLOAT; break;
	case 9: type = SQL_BIT; break;
	case 10: type = SQL_WCHAR; break;
	case 11: type = SQL_WVARCHAR; break;
	case 12: type = SQL_DATE; break;
	case 13: type = SQL_TIME; break;
	case 14: type = SQL_BINARY; break;
	case 15: type = SQL_VARBINARY; break;
	case 16: 
	case 17:
	    type = SQL_LONGVARBINARY; break;
	default: type = SQL_ALL_TYPES;
	}

	res = SQLGetTypeInfo(thisHandle->hStmt, type);
	success_or_failure(_("[RODBC] ERROR: SQLTables failed"));
    }
    return ScalarLogical(stat);
}

SEXP RODBCGetInfo(SEXP chan)
{
    SEXP ans;
    int i;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    char buf[1000];
    SQLSMALLINT nbytes;
    SQLRETURN retval;
    SQLSMALLINT InfoTypes[] = 
	{SQL_DBMS_NAME, SQL_DBMS_VER, SQL_DRIVER_ODBC_VER,
	 SQL_DATA_SOURCE_NAME, SQL_DRIVER_NAME, SQL_DRIVER_VER,
	 SQL_ODBC_VER, SQL_SERVER_NAME};

    /* Rprintf("using (%p, %p)\n", chan, thisHandle); */
    PROTECT(ans = allocVector(STRSXP, 8));
    for (i = 0; i < LENGTH(ans); i++) {
	retval = SQLGetInfo(thisHandle->hDbc,
			    InfoTypes[i], buf, (SQLSMALLINT)1000, &nbytes);
	if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	    geterr(thisHandle);
	    SET_STRING_ELT(ans, i, mkChar("error"));
	    break;
	} else
	    SET_STRING_ELT(ans, i, mkChar(buf));
    }
    UNPROTECT(1);
    return ans;
}

static void cachenbind_free(pRODBCHandle thisHandle)
{
    SQLUSMALLINT i;
    if(thisHandle->ColData) {
	for (i = 0; i < thisHandle->nAllocated; i++)
	    if(thisHandle->ColData[i].pData)
		Free(thisHandle->ColData[i].pData);
	Free(thisHandle->ColData);
	thisHandle->ColData = NULL; /* to be sure */
    }   
}

#define BIND(type, buf, size) \
	    retval = SQLBindCol(thisHandle->hStmt, i+1, type,\
				thisHandle->ColData[i].buf, size,\
				thisHandle->ColData[i].IndPtr);\
            break;


/*      *******************************************
 *
 *	Common column cache and bind for query-like routines
 *      This is used to bind the columns for all queries that
 *      produce a result set, which is uses by RODBCFetchRows.
 *
 *	*******************************************/
/* returns 1 for success, -1 for failure */
static int cachenbind(pRODBCHandle thisHandle, int nRows)
{
    SQLUSMALLINT i;
    SQLRETURN retval;

    /* Now cache the number of columns, rows */
    retval = SQLNumResultCols(thisHandle->hStmt, &NCOLS);
    if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	/* assume this is not an error but that no rows found */
	NROWS = 0;
	return 1 ;
    }
    retval = SQLRowCount(thisHandle->hStmt, &NROWS);
    if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	geterr(thisHandle);
	errlistAppend(thisHandle, _("[RODBC] ERROR: SQLRowCount failed"));
	goto error;
    }
    /* Allocate storage for ColData array,
       first freeing what was there before */
    cachenbind_free(thisHandle);
    thisHandle->ColData = Calloc(NCOLS, COLUMNS);
    /* this allocates Data as zero */
    thisHandle->nAllocated = NCOLS;

    /* attempt to set the row array size */
    thisHandle->rowArraySize = my_min(nRows, MAX_ROWS_FETCH);

    /* passing unsigned integer values via casts is a bad idea.
       But here double casting works because long and a pointer
       are the same size on all relevant platforms (since
       Win64 is not relevant). */
    retval = SQLSetStmtAttr(thisHandle->hStmt, SQL_ATTR_ROW_ARRAY_SIZE,
			    (SQLPOINTER) (unsigned long) thisHandle->rowArraySize, 0 );
    if (retval != SQL_SUCCESS) thisHandle->rowArraySize = 1;
    thisHandle->rowsUsed = 0;

    /* Set pointer to report number of rows fetched
     */

    if (thisHandle->rowArraySize != 1) {
	retval = SQLSetStmtAttr(thisHandle->hStmt,
				SQL_ATTR_ROWS_FETCHED_PTR,
				&thisHandle->rowsFetched, 0);
	if (retval != SQL_SUCCESS) {
	    thisHandle->rowArraySize = 1;
	    SQLSetStmtAttr(thisHandle->hStmt, SQL_ATTR_ROW_ARRAY_SIZE,
			   (SQLPOINTER) 1, 0 );
	}
    }
    nRows = thisHandle->rowArraySize;

    /* step through each col and cache metadata: cols are numbered from 1!
     */
    for (i = 0; i < NCOLS; i++) {
	retval = SQLDescribeCol(thisHandle->hStmt, i+1,
				thisHandle->ColData[i].ColName, 256,
				&thisHandle->ColData[i].NameLength,
				&thisHandle->ColData[i].DataType,
				&thisHandle->ColData[i].ColSize,
				&thisHandle->ColData[i].DecimalDigits,
				&thisHandle->ColData[i].Nullable);
	if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	    geterr(thisHandle);
	    errlistAppend(thisHandle, 
			  _("[RODBC] ERROR: SQLDescribeCol failed"));
	    goto error;
	}
	/* now bind the col to its data buffer */
	/* MSDN say the BufferLength is ignored for fixed-size
	   types, but this is not so for UnixODBC */
	/* We could add other types here, in particular
	   SQL_C_USHORT
	   SQL_C_ULONG (map to double)
	   SQL_C_BIT
	   SQL_C_WCHAR (map to UTF-8)
	 */
	switch(thisHandle->ColData[i].DataType) {
	case SQL_DOUBLE:
	    BIND(SQL_C_DOUBLE, RData, sizeof(double));
	case SQL_REAL:
	    BIND(SQL_C_FLOAT, R4Data, sizeof(float));
	case SQL_INTEGER:
	    BIND(SQL_C_SLONG, IData, sizeof(int));
	case SQL_SMALLINT:
	    BIND(SQL_C_SSHORT, I2Data, sizeof(short));
	case SQL_BINARY:
	case SQL_VARBINARY:
	case SQL_LONGVARBINARY:
	{
	    /* should really use SQLCHAR (unsigned) */
	    SQLLEN datalen = thisHandle->ColData[i].ColSize;
	    thisHandle->ColData[i].datalen = datalen;
	    thisHandle->ColData[i].pData =
		Calloc(nRows * (datalen + 1), char);
	    BIND(SQL_C_BINARY, pData, datalen);
	}
	default:
	{
	    SQLLEN datalen = thisHandle->ColData[i].ColSize;
	    if (datalen <= 0 || datalen < COLMAX) datalen = COLMAX;
	    /* sanity check as the reports are sometimes unreliable */
	    if (datalen > 65535) datalen = 65535;
	    thisHandle->ColData[i].pData =
		Calloc(nRows * (datalen + 1), char);
	    thisHandle->ColData[i].datalen = datalen;
	    BIND(SQL_C_CHAR, pData, datalen);
	}
	}

	if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	    geterr(thisHandle);
	    errlistAppend(thisHandle, _("[RODBC] ERROR: SQLBindCol failed"));
	    goto error;
	}
    }
    return 1;
error:
    (void)SQLFreeStmt(thisHandle->hStmt, SQL_CLOSE);
    (void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);
    thisHandle->hStmt = NULL;
    return -1;
}

/***************************************/

SEXP RODBCNumCols(SEXP chan)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);

    if(NCOLS == -1)
	errlistAppend(thisHandle, _("[RODBC] No results available"));

    return ScalarInteger((int) NCOLS);
}

#define ROWSNA (SQLLEN) -1

static SEXP mkRaw(char *ptr, unsigned int len)
{
    SEXP ans = allocVector(RAWSXP, len);
    memcpy(RAW(ans), ptr, len);
    return ans;
}


SEXP RODBCFetchRows(SEXP chan, SEXP max, SEXP bs, SEXP nas, SEXP believeNRows)
{
    int stat = 1, i, j, blksize, nc, n, row;
    int maximum = asInteger(max);
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    int useNRows = asLogical(believeNRows) != 0;
    int buffsize = asInteger(bs);
    SEXP data, names, ans;
    SQLRETURN retval;

    nc = NCOLS;

    PROTECT(ans = allocVector(VECSXP, 2)); /* create answer [0] = data, [1]=stat */
    if(!useNRows || !thisHandle->useNRows) NROWS = ROWSNA;
    if(nc == 0) stat = -2;

    if(nc == -1) {
	errlistAppend(thisHandle, _("[RODBC] No results available"));
	stat = -1;
    }

    if(stat < 0 || nc == 0) {
	if(NROWS == 0)
	    errlistAppend(thisHandle, _("No Data"));
	PROTECT(data = allocVector(VECSXP, 0));
    } else { /* NCOLS > 0 */
	PROTECT(data = allocVector(VECSXP, nc));

	if(NROWS == ROWSNA) {
	    if(maximum) blksize = maximum;
	    else {
		maximum = INT_MAX;
		blksize = (buffsize < 100) ? 100: buffsize;
	    }
	} else {
	    if(!maximum || maximum > NROWS) maximum = NROWS;
	    blksize = maximum;
	}
	for(i = 0; i < nc; i++) {
	    switch(thisHandle->ColData[i].DataType) {
	    case SQL_DOUBLE:
	    case SQL_REAL:
		SET_VECTOR_ELT(data, i, allocVector(REALSXP, blksize));
		break;
	    case SQL_INTEGER:
	    case SQL_SMALLINT:
		SET_VECTOR_ELT(data, i, allocVector(INTSXP, blksize));
		break;
	    case SQL_BINARY:
	    case SQL_VARBINARY:
	    case SQL_LONGVARBINARY:
		SET_VECTOR_ELT(data, i, allocVector(VECSXP, blksize));
		break;
	    default:
		SET_VECTOR_ELT(data, i, allocVector(STRSXP, blksize));
	    }
	}

	for(j = 1; j <= maximum; ) {
	    if(j > blksize) {
		blksize *= 2;
		for (i = 0; i < nc; i++)
		    SET_VECTOR_ELT(data, i,
				   lengthgets(VECTOR_ELT(data, i), blksize));
	    }
	    if(thisHandle->rowsUsed == 0L ||
	       thisHandle->rowsUsed >= thisHandle->rowsFetched) {
		if (thisHandle->rowArraySize == 1) {
		    retval = SQLFetch(thisHandle->hStmt);
		    thisHandle->rowsFetched = 1;
		} else
		    /* this will update thisHandle->rowsFetched */
		    retval = SQLFetchScroll(thisHandle->hStmt, SQL_FETCH_NEXT, 
					    0);
		if(retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO)
		    break;
		thisHandle->rowsUsed = 0;
		/* SQL_SUCCESS_WITH_INFO if char column(s) truncated */
		if(retval == SQL_SUCCESS_WITH_INFO) {
		    SQLCHAR sqlstate[6], msg[SQL_MAX_MESSAGE_LENGTH];
		    SQLINTEGER NativeError;
		    SQLSMALLINT MsgLen;
		    if(SQLError(hEnv, thisHandle->hDbc,
				thisHandle->hStmt, sqlstate, &NativeError,
				msg, (SQLSMALLINT)sizeof(msg), &MsgLen)
		       == SQL_SUCCESS) {
			if(strcmp((char *)sqlstate, "O1004") == 0)
			    warning(_("character data truncated in column '%s'"),
				    (char *)thisHandle->ColData[i].ColName);
		    }
		}
	    }

	    for(row = thisHandle->rowsUsed;
		row < thisHandle->rowsFetched && j <= maximum;
		j++, row++)
	    {
		thisHandle->rowsUsed++;
		if(j > blksize) {
		    blksize *= 2;
		    for (i = 0; i < nc; i++)
			SET_VECTOR_ELT(data, i,
				       lengthgets(VECTOR_ELT(data, i), blksize));
		}
		for (i = 0; i < nc; i++) {
		    SQLLEN len = thisHandle->ColData[i].IndPtr[row];
		    switch(thisHandle->ColData[i].DataType) {
		    case SQL_DOUBLE:
			REAL(VECTOR_ELT(data, i))[j-1] = 
			    len == SQL_NULL_DATA ? NA_REAL :
			    thisHandle->ColData[i].RData[row];
			break;
		    case SQL_REAL:
			REAL(VECTOR_ELT(data, i))[j-1] = 
			    len == SQL_NULL_DATA ? NA_REAL :
			    (double) thisHandle->ColData[i].R4Data[row];
			break;
		    case SQL_INTEGER:
			INTEGER(VECTOR_ELT(data, i))[j-1] = 
			    len == SQL_NULL_DATA ? NA_INTEGER :
			    thisHandle->ColData[i].IData[row];
			break;
		    case SQL_SMALLINT:
			INTEGER(VECTOR_ELT(data, i))[j-1] = 
			    len == SQL_NULL_DATA ? NA_INTEGER :
			    (int) thisHandle->ColData[i].I2Data[row];
			break;
		    case SQL_BINARY:
		    case SQL_VARBINARY:
		    case SQL_LONGVARBINARY:
		    {
			SEXP this = (len == SQL_NULL_DATA) ?
			    allocVector(RAWSXP, 0) :
			    mkRaw(thisHandle->ColData[i].pData + 
				  (thisHandle->ColData[i].datalen * row), len);
			SET_VECTOR_ELT(VECTOR_ELT(data, i), j-1, this);
			break;
		    }
		    default:
			SET_STRING_ELT(VECTOR_ELT(data, i), j-1,
				       len == SQL_NULL_DATA ?
				       STRING_ELT(nas, 0) :
				       mkChar(thisHandle->ColData[i].pData + 
					      (thisHandle->ColData[i].datalen * row)));
		    }
		}
	    }
	}

	n = --j;
	if (n > 0 && !(maximum && n >= maximum)) {
	    /* means 'no further results available' */
	    NCOLS = -1;
	    thisHandle->rowsUsed = 0; /* safety */
	    /* so we can free the buffers */
	    cachenbind_free(thisHandle);
	}
	
	if (n < blksize) { /* need to trim vectors */
	    for (i = 0; i < nc; i++)
		SET_VECTOR_ELT(data, i,
			       lengthgets(VECTOR_ELT(data, i), n));
	}
    }

    SET_VECTOR_ELT(ans, 0, data);
    SET_VECTOR_ELT(ans, 1, ScalarInteger(stat));
    PROTECT(names = allocVector(STRSXP, 2));
    SET_STRING_ELT(names, 0, mkChar("data"));
    SET_STRING_ELT(names, 1, mkChar("stat"));
    SET_NAMES(ans, names);
    UNPROTECT(3); /* ans data names */
    return ans;
}


/**********************************************************************/

SEXP RODBCColData(SEXP chan)
{
    SEXP ans, names, type, ansnames;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    int i, nc;

    PROTECT(ans = allocVector(VECSXP, 2));
    if(NCOLS == -1)
	errlistAppend(thisHandle, _("[RODBC] No results available"));
    nc = NCOLS;
    if(nc < 0) nc = 0;
    SET_VECTOR_ELT(ans, 0, names = allocVector(STRSXP, nc));
    SET_VECTOR_ELT(ans, 1, type = allocVector(STRSXP, nc));
    PROTECT(ansnames = allocVector(STRSXP, 2));
    SET_STRING_ELT(ansnames, 0, mkChar("names"));
    SET_STRING_ELT(ansnames, 1, mkChar("type"));
    setAttrib(ans, R_NamesSymbol, ansnames);

    for (i = 0; i < nc; i++) {
	SET_STRING_ELT(names, i,
		       mkChar((char *)thisHandle->ColData[i].ColName));
	/* at the moment we only need to know if it is 
	   a date, datetime or other */ 
	switch(thisHandle->ColData[i].DataType) {
	case SQL_CHAR: SET_STRING_ELT(type, i, mkChar("char")); break;
	case SQL_NUMERIC: SET_STRING_ELT(type, i, mkChar("numeric")); break;
	case SQL_DECIMAL: SET_STRING_ELT(type, i, mkChar("decimal")); break;
	case SQL_INTEGER: SET_STRING_ELT(type, i, mkChar("int")); break;
	case SQL_SMALLINT: SET_STRING_ELT(type, i, mkChar("smallint")); break;
	case SQL_FLOAT: SET_STRING_ELT(type, i, mkChar("float")); break;
	case SQL_REAL: SET_STRING_ELT(type, i, mkChar("real")); break;
	case SQL_DOUBLE: SET_STRING_ELT(type, i, mkChar("double")); break;
	case SQL_DATE:
	case SQL_TYPE_DATE: 
	    SET_STRING_ELT(type, i, mkChar("date")); break;
	case SQL_TIME:
	case SQL_TYPE_TIME:
	    SET_STRING_ELT(type, i, mkChar("time")); break;
	case SQL_TIMESTAMP:
	case SQL_TYPE_TIMESTAMP:
	    SET_STRING_ELT(type, i, mkChar("timestamp")); break;
	case SQL_VARCHAR:
	    SET_STRING_ELT(type, i, mkChar("varchar")); break;
	case SQL_BINARY:
	case SQL_VARBINARY:
	case SQL_LONGVARBINARY:
	    SET_STRING_ELT(type, i, mkChar("varchar")); break;
	case SQL_UNKNOWN_TYPE:
	default:
	    SET_STRING_ELT(type, i, mkChar("unknown"));
	}
    }
    UNPROTECT(2);
    return ans;
}

/*********************************************************/

SEXP RODBCUpdate(SEXP chan, SEXP query, SEXP data, SEXP dataseq, 
		 SEXP params, SEXP test)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    int rows, i, j, k, stat = 1, vtest = asInteger(test), nparams;
    int *sequence = INTEGER(dataseq);
    const char *cquery = translateChar(STRING_ELT(query, 0));
    SQLRETURN res = 0; /* -Wall */
    SEXP paramnames = VECTOR_ELT(params, 0);

    nparams = length(paramnames);
    NCOLS = nparams;

    clearresults(thisHandle);

    res = SQLAllocHandle(SQL_HANDLE_STMT, thisHandle->hDbc, &thisHandle->hStmt);
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	errlistAppend(thisHandle, err_SQLAllocStmt);
	stat = -1;
	goto end;
    }
    res = SQLPrepare( thisHandle->hStmt, (SQLCHAR *) cquery,
		      strlen(cquery) );
    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
	char *message = Calloc(strlen(cquery)+50, char);
	sprintf(message,
		"[RODBC] ERROR: Could not SQLPrepare '%s'", cquery);
	geterr(thisHandle);
	errlistAppend(thisHandle, message);
	(void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);
	thisHandle->hStmt = NULL;
	stat = -1;
	goto end;
    }
    /* Allocate storage for ColData array,
       first freeing what was there last */
    cachenbind_free(thisHandle);
    thisHandle->ColData = Calloc(NCOLS, COLUMNS);
    /* this allocates Data as zero */
    thisHandle->nAllocated = NCOLS;

    /* extract the column data  */
    for(j = 0; j < nparams; j++) {
	int tmp;
	strcpy((char *) thisHandle->ColData[j].ColName,
	       translateChar(STRING_ELT(paramnames, j)));
	thisHandle->ColData[j].DataType = INTEGER(VECTOR_ELT(params, 1))[j];
	thisHandle->ColData[j].ColSize = INTEGER(VECTOR_ELT(params, 2))[j];
	/* I don't think this would be NA, but the code was here */
	tmp  = INTEGER(VECTOR_ELT(params, 3))[j];
	thisHandle->ColData[j].DecimalDigits = (tmp == NA_INTEGER) ? 0 : tmp;

	if(vtest)
	    Rprintf("Binding: '%s' DataType %d, ColSize %d\n",
		    (char *) thisHandle->ColData[j].ColName,
		    thisHandle->ColData[j].DataType,
		    thisHandle->ColData[j].ColSize);
	switch(TYPEOF(VECTOR_ELT(data, sequence[j]))) {
	case REALSXP:
	    /* It is possible that we are sending data to a DECIMAL or
	       NUMERIC data type, when DecimalDigits would be relevant */
	    res = SQLBindParameter(thisHandle->hStmt,
				   j+1, SQL_PARAM_INPUT, SQL_C_DOUBLE,
				   thisHandle->ColData[j].DataType,
				   thisHandle->ColData[j].ColSize,
				   thisHandle->ColData[j].DecimalDigits,
				   thisHandle->ColData[j].RData,
				   0,
				   thisHandle->ColData[j].IndPtr);
	    break;
	case INTSXP:
	    res = SQLBindParameter(thisHandle->hStmt,
				   j+1, SQL_PARAM_INPUT, SQL_C_SLONG,
				   thisHandle->ColData[j].DataType,
				   thisHandle->ColData[j].ColSize,
				   thisHandle->ColData[j].DecimalDigits,
				   thisHandle->ColData[j].IData,
				   0,
				   thisHandle->ColData[j].IndPtr);
	    break;
	default:
	{
	    int datalen = thisHandle->ColData[j].ColSize;
	    /* Why change here but not when sending the data? */
	    if (datalen <= 0) datalen = 1024;
	    thisHandle->ColData[j].pData = Calloc(datalen+1, char);
	    res = SQLBindParameter(thisHandle->hStmt,
				   j+1, SQL_PARAM_INPUT, SQL_C_CHAR,
				   thisHandle->ColData[j].DataType,
				   datalen,
				   thisHandle->ColData[j].DecimalDigits,
				   thisHandle->ColData[j].pData,
				   0,
				   thisHandle->ColData[j].IndPtr);
	}
	}

	if(res  != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO) {
	    geterr(thisHandle);
	    errlistAppend(thisHandle, _("[RODBC] SQLBindParameter failed"));
	    geterr(thisHandle);
	    stat = -1;
	    goto end;
	}
    }

    /* now send the data */
    if(vtest) Rprintf("Parameters:\n");
    rows = LENGTH(VECTOR_ELT(data, 0));
    for(i = 0; i < rows; i++) {
	for(j = 0; j < LENGTH(data); j++) {
	    k = sequence[j]; /* get the right column */
	    switch(TYPEOF(VECTOR_ELT(data, k))) { 
	    case REALSXP:
		thisHandle->ColData[j].RData[0] =
		    REAL(VECTOR_ELT(data, k))[i];
		if(vtest)
		    Rprintf("no: %d: %s %g/***/", j + 1,
			    (char *) thisHandle->ColData[j].ColName,
			    REAL(VECTOR_ELT(data, k))[i]);
		if(ISNAN(REAL(VECTOR_ELT(data, k))[i]))
		    thisHandle->ColData[j].IndPtr[0] = SQL_NULL_DATA;
		else
		    thisHandle->ColData[j].IndPtr[0] = SQL_NTS;
		break;
	    case INTSXP:
		thisHandle->ColData[j].IData[0] =
		    INTEGER(VECTOR_ELT(data, k))[i];
		if(vtest)
		    Rprintf("no: %d: %s %d/***/", j + 1,
			    (char *) thisHandle->ColData[j].ColName,
			    INTEGER(VECTOR_ELT(data, k))[i]);
		if(INTEGER(VECTOR_ELT(data, k))[i] == NA_INTEGER)
		    thisHandle->ColData[j].IndPtr[0] = SQL_NULL_DATA;
		else
		    thisHandle->ColData[j].IndPtr[0] = SQL_NTS;
		break;
	    default:
	    {
		const char *cData = translateChar(STRING_ELT(VECTOR_ELT(data, k), i));
		int datalen = thisHandle->ColData[j].ColSize;
		strncpy(thisHandle->ColData[j].pData, cData, datalen);
		thisHandle->ColData[j].pData[datalen] = '\0';
		if(strlen(cData) > datalen)
		    warning(_("character data '%s' truncated to %d bytes in column '%s'"),
			    cData, datalen, (char *) thisHandle->ColData[j].ColName);
		if(vtest)
		    Rprintf("no: %d: %s %s/***/", j + 1,
			    (char *) thisHandle->ColData[j].ColName,
			    cData);
		if(STRING_ELT(VECTOR_ELT(data, k), i) == NA_STRING)
		    thisHandle->ColData[j].IndPtr[0] = SQL_NULL_DATA;
		else
		    thisHandle->ColData[j].IndPtr[0] = SQL_NTS;
	    }
	    }
	}
	if(vtest) Rprintf("\n");
	if(vtest < 2) {
	    res = SQLExecute(thisHandle->hStmt);
	    if( res != SQL_SUCCESS && res != SQL_SUCCESS_WITH_INFO ) {
		errlistAppend(thisHandle, _("[RODBC] Failed exec in Update"));
		geterr(thisHandle);
		stat = -1;
		goto end;
	    }
	}
    }
end:
    /* free the buffers */
    cachenbind_free(thisHandle);
    (void)SQLFreeStmt(thisHandle->hStmt, SQL_RESET_PARAMS);
    (void)SQLFreeHandle(SQL_HANDLE_STMT, thisHandle->hStmt);
    thisHandle->hStmt = NULL;
    return ScalarInteger(stat);
}

/************************************************
 *
 *		DISCONNECT
 *
 * **********************************************/

static int inRODBCClose(pRODBCHandle thisHandle)
{
    int success = 1;
    SQLRETURN retval;

    /* Rprintf("closing %p\n", thisHandle); */
    if(thisHandle->channel <= MAX_CHANNELS) opened_handles[thisHandle->channel] = NULL;
    retval = SQLDisconnect( thisHandle->hDbc );
    if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	/* was errlist_append, but errorlist is squashed before return! */
	warning(_("[RODBC] Error in SQLDisconnect"));
	success = -1;
    }
    retval = SQLFreeHandle(SQL_HANDLE_DBC, thisHandle->hDbc);
    if( retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO ) {
	warning(_("[RODBC] Error in SQLFreeconnect"));
	success = -1;
    }
    cachenbind_free(thisHandle);
    errorFree(thisHandle->msglist);
    R_ClearExternalPtr(thisHandle->extPtr);
    Free(thisHandle);
    return success;
}

SEXP RODBCClose(SEXP chan)
{
    return ScalarInteger( inRODBCClose(R_ExternalPtrAddr(chan)));
}

static void chanFinalizer(SEXP ptr)
{
    if(!R_ExternalPtrAddr(ptr)) return;
    /* Rprintf("finalizing %p\n", R_ExternalPtrAddr(ptr)); */
    warning(_("closing unused RODBC handle %d\n"),
	    ((pRODBCHandle )R_ExternalPtrAddr(ptr))->channel);
    inRODBCClose(R_ExternalPtrAddr(ptr));
    R_ClearExternalPtr(ptr); /* not really needed */
}


SEXP RODBCCloseAll(void)
{
    int i;

    for(i = 1; i <= my_min(nChannels, MAX_CHANNELS); i++)
	if(opened_handles[i])
	    inRODBCClose(opened_handles[i]);

    return R_NilValue;
}

/**********************************************************
 *
 * Some utility routines to build, count, read and free a linked list
 * of diagnostic record messages
 * This is implemented as a linked list against the possibility
 * of using SQLGetDiagRec which returns an unknown number of messages.
 *
 * Don't use while !SQL_NO_DATA 'cause iodbc does not support it
 *****************************************/
static void
geterr(pRODBCHandle thisHandle)
{
    SQLCHAR sqlstate[6], msg[SQL_MAX_MESSAGE_LENGTH];
    SQLINTEGER NativeError;
    SQLSMALLINT i = 1, MsgLen;
    char message[SQL_MAX_MESSAGE_LENGTH+16];
    SQLRETURN retval;

    while(1) {	/* exit via break */
	retval =  SQLGetDiagRec(SQL_HANDLE_STMT,
				thisHandle->hStmt, i++,
				sqlstate, &NativeError, msg, sizeof(msg),
				&MsgLen);

	if(retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO) break;
	sprintf(message,"%s %d %s", sqlstate, (int)NativeError, msg);
	errlistAppend(thisHandle, message);
	// i++;
    }
}

/****************************************
 * append to list
 */

/* Can't mix strdup and R's memory allocation */
static char *mystrdup(const char *s)
{
    char *s2;
    s2 = Calloc(strlen(s) + 1, char);
    strcpy(s2, s);
    return s2;
}


static void errlistAppend(pRODBCHandle thisHandle, const char *string)
{
    SQLMSG *root;
    SQLCHAR *buffer;

/* do this strdup so that all the message chain can be freed*/
    if((buffer = (SQLCHAR *) mystrdup(string)) == NULL) {
	REprintf("RODBC.c: Memory Allocation failure for message string\n");
	return;
    }
    root = thisHandle->msglist;

    if(root) {
	while(root->message) {
	    if(root->next) root = root->next;
	    else break;
	}
	root->next = Calloc(1, SQLMSG);
	root = root->next;
    } else {
	root = thisHandle->msglist = Calloc(1, SQLMSG);
    }
    root->next = NULL;
    root->message = buffer;
}




/***************************************/
/* currently unused */
SEXP RODBCErrMsgCount(SEXP chan)
{
    int i = 0;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLMSG *root;

    root = thisHandle->msglist;
    if(root) {
	while(root->message) {
	    i++;
	    if(root->next)
		root=root->next;
	    else break;
	}
    }
    return ScalarInteger(i);
}

/******************************/

SEXP RODBCGetErrMsg(SEXP chan)
{
    SEXP ans;
    int i, num;
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    SQLMSG *root;

    /* count the messages */
    i = 0;
    root = thisHandle->msglist;
    if(root) {
	while(root->message) {
	    i++;
	    if(root->next)
		root = root->next;
	    else break;
	}
    }
    num = i; i = 0;
    PROTECT(ans = allocVector(STRSXP, num));
    root = thisHandle->msglist;
    if(root) {
	while(root->message) {
	    SET_STRING_ELT(ans, i++, mkChar((char *)root->message));
	    if(root->next)
		root = root->next;
	    else break;
	}
    }
    UNPROTECT(1);
    return ans;
}

/********/

SEXP RODBCClearError(SEXP chan)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);

    errorFree(thisHandle->msglist);
    thisHandle->msglist = NULL;
    return R_NilValue;
}

/*********************/

static void errorFree(SQLMSG *node)
{
    if(!node) return;
    if(node->next)
	errorFree(node->next);
    if(node) {
	Free(node->message);
	Free(node);
	node = NULL;
    }
}

/**********************
 * Check for valid channel since invalid
 * will cause segfault on most functions
 */

SEXP RODBCcheckchannel(SEXP chan, SEXP id)
{
    SEXP ptr = getAttrib(chan, install("handle_ptr"));
    pRODBCHandle thisHandle = R_ExternalPtrAddr(ptr);

    return ScalarLogical(thisHandle && TYPEOF(ptr) == EXTPTRSXP &&
			 thisHandle->channel == asInteger(chan) &&
			 thisHandle->id == asInteger(id));
}

/***********************
 * Set connection auto-commit mode
 */
SEXP RODBCSetAutoCommit(SEXP chan, SEXP autoCommit)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    int iAutoCommit = asLogical(autoCommit) != 0;
    int rc;

    if (!iAutoCommit)
	rc = SQLSetConnectAttr(thisHandle->hDbc, SQL_ATTR_AUTOCOMMIT,
			       (SQLPOINTER) (unsigned long) SQL_AUTOCOMMIT_OFF,
			       0);
    else
	rc = SQLSetConnectAttr(thisHandle->hDbc, SQL_ATTR_AUTOCOMMIT,
			       (SQLPOINTER) (unsigned long) SQL_AUTOCOMMIT_ON,
			       0);
    return ScalarInteger(rc);
}

/***********************
 * Commit or rollback a transaction
 */
SEXP RODBCEndTran(SEXP chan, SEXP sCommit)
{
    pRODBCHandle thisHandle = R_ExternalPtrAddr(chan);
    int rc;

    rc = SQLEndTran(SQL_HANDLE_DBC, thisHandle->hDbc,
		    (asLogical(sCommit) != 0) ? SQL_COMMIT : SQL_ROLLBACK);
    return ScalarInteger(rc);
}


SEXP RODBCListDataSources(SEXP stype)
{
    SEXP ans, nm;
    PROTECT_INDEX pidx, nidx;

    UWORD fDirection = SQL_FETCH_FIRST;
    SQLRETURN retval;
    SQLCHAR szDSN[SQL_MAX_DSN_LENGTH+1];
    SQLCHAR szDescription[100];
    char message[SQL_MAX_DSN_LENGTH+101];
    int i = 0, ni = 100, type = asInteger(stype);

    odbcInit();
    switch(type) {
    case 2:  fDirection = SQL_FETCH_FIRST_USER; break;
    case 3:  fDirection = SQL_FETCH_FIRST_SYSTEM; break;
    default: fDirection = SQL_FETCH_FIRST; break;
    }

    PROTECT_WITH_INDEX(ans = allocVector(STRSXP, ni), &pidx);
    PROTECT_WITH_INDEX(nm = allocVector(STRSXP, ni), &nidx);
    do {
	retval = SQLDataSources(hEnv, fDirection,
				(UCHAR *)szDSN, sizeof(szDSN), NULL,
				(UCHAR *)szDescription,
				sizeof(szDescription), NULL);
	if(retval == SQL_NO_DATA) break;
	if(retval != SQL_SUCCESS && retval != SQL_SUCCESS_WITH_INFO) {
	    sprintf(message, "SQLDataSources returned: %d", retval);
	    SET_STRING_ELT(ans, i, mkChar(message));
	} else {
	    SET_STRING_ELT(nm, i, mkChar((char *)szDSN));
	    SET_STRING_ELT(ans, i, mkChar((char *)szDescription));
	}
	fDirection = SQL_FETCH_NEXT;
	i++;
	if(i >= ni - 1) {
	    ni *= 2;
	    REPROTECT(ans = lengthgets(ans, ni), pidx);
	    REPROTECT(nm = lengthgets(nm, ni), nidx);
	}
    } while(retval == SQL_SUCCESS || retval == SQL_SUCCESS_WITH_INFO);

    REPROTECT(ans = lengthgets(ans, i), pidx);
    REPROTECT(nm = lengthgets(nm, i), nidx);
    setAttrib(ans, R_NamesSymbol, nm);
    UNPROTECT(2);
    return ans;
}

#include <R_ext/Rdynload.h>

static const R_CallMethodDef CallEntries[] = {
    {"RODBCGetErrMsg", (DL_FUNC) &RODBCGetErrMsg, 1},
    {"RODBCClearError", (DL_FUNC) &RODBCClearError, 1},
    {"RODBCDriverConnect", (DL_FUNC) &RODBCDriverConnect, 4},
    {"RODBCQuery", (DL_FUNC) &RODBCQuery, 3},
    {"RODBCUpdate", (DL_FUNC) &RODBCUpdate, 6},
    {"RODBCTables", (DL_FUNC) &RODBCTables, 6},
    {"RODBCColumns", (DL_FUNC) &RODBCColumns, 5},
    {"RODBCSpecialColumns", (DL_FUNC) &RODBCSpecialColumns, 4},
    {"RODBCPrimaryKeys", (DL_FUNC) &RODBCPrimaryKeys, 4},
    {"RODBCColData", (DL_FUNC) &RODBCColData, 1},
    {"RODBCNumCols", (DL_FUNC) &RODBCNumCols, 1},
    {"RODBCClose", (DL_FUNC) &RODBCClose, 1},
    {"RODBCCloseAll", (DL_FUNC) &RODBCCloseAll, 0},
    {"RODBCFetchRows", (DL_FUNC) &RODBCFetchRows, 5},
    {"RODBCGetInfo", (DL_FUNC) &RODBCGetInfo, 1},
    {"RODBCcheckchannel", (DL_FUNC) &RODBCcheckchannel, 2},
    {"RODBCclearresults", (DL_FUNC) &RODBCclearresults, 1},
    {"RODBCSetAutoCommit", (DL_FUNC) &RODBCSetAutoCommit, 2},
    {"RODBCEndTran", (DL_FUNC) &RODBCEndTran, 2},
    {"RODBCTypeInfo", (DL_FUNC) &RODBCTypeInfo, 2},
    {"RODBCListDataSources", (DL_FUNC) &RODBCListDataSources, 1},
    {"RODBCTerm", (DL_FUNC) &RODBCTerm, 0},
    {NULL, NULL, 0}
};

void R_init_RODBC(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
    R_forceSymbols(dll, TRUE);
}
