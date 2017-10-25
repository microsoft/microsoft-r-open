# file RODBC/R/RODBC.R
# copyright (C) 1999-2002  M. Lapsley
# copyright (C) 2002-2016  B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#
#  Low level wrappers for odbc driver
#
#
#
.onLoad <- function(libname, pkgname)
{
    if(is.null(getOption("dec")))
        options(dec = Sys.localeconv()["decimal_point"])
}

.onUnload <- function(libpath)
{
    odbcCloseAll()
    .Call(C_RODBCTerm)
    library.dynam.unload("RODBC", libpath)
}

odbcGetErrMsg <- function(channel)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    err <- .Call(C_RODBCGetErrMsg, attr(channel, "handle_ptr"))
    .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
    return(err)
}

odbcClearError <- function(channel)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    .Call(C_RODBCClearError, attr(channel, "handle_ptr"))
    invisible()
}

odbcReConnect <- function(channel, ...)
{
    if(!inherits(channel, "RODBC"))
        stop("Argument 'channel' must inherit from class RODBC")
    Call <- attr(channel, "call")
    dots <- list(...)
    if("uid" %in% names(dots)) {
        uid <- dots$uid; dots$uid <- NULL
        Call$Connection <- sub("UID=[^;]+($|;)",
                               paste("UID=", uid, ";", sep=""),
                               Call$connection)
    }
    if("pwd" %in% names(dots)) {
        pwd <- dots$pwd; dots$pwd <- NULL
        Call$connection <- sub("PWD=[^;]+($|;)",
                               paste("PWD=", pwd, ";", sep=""),
                               Call$connection)
    }
    if(length(dots)) Call[names(dots)] <- dots
    eval.parent(Call)
}


odbcConnect <- function (dsn, uid = "", pwd = "", ...)
{
    Call <- match.call(); Call$uid <- Call$pwd <- NULL
    Call[[1]] <- quote(RODBC::odbcDriverConnect)
    st <- paste("DSN=", dsn, sep="")
    if(nchar(uid)) st <- paste(st, ";UID=", uid, sep="")
    if(nchar(pwd)) st <- paste(st, ";PWD=", pwd, sep="")
    Call[[2]] <- st; names(Call)[2] <- ""
    eval.parent(Call)
}

odbcDriverConnect <-
    function (connection = "", case = "nochange", believeNRows = TRUE,
              colQuote, tabQuote = colQuote, interpretDot = TRUE,
              DBMSencoding = "", rows_at_time = 100, readOnlyOptimize = FALSE)
{
   id <- as.integer(1 + runif(1, 0, 1e5))
   stat <- .Call(C_RODBCDriverConnect, as.character(connection), id,
                 as.integer(believeNRows), as.logical(readOnlyOptimize))
   if(stat < 0L) {
       warning("ODBC connection failed")
       return(stat)
   }
   Call <- match.call()
   res <- .Call(C_RODBCGetInfo, attr(stat, "handle_ptr"))
   isMySQL <- res[1L] == "MySQL"
   if(missing(colQuote)) colQuote <- ifelse(isMySQL, "`", '"')
   if(missing(case))
       case <- switch(res[1L],
                      "MySQL" = "mysql",
                      "PostgreSQL" = "postgresql",
                      "nochange")
   switch(case,
	toupper = case <- 1L,
	tolower = case <- 2L,
	postgresql = case <- 2L,
	nochange = case <- 0L,
	msaccess = case <- 0L,
	mysql = case <- ifelse(.Platform$OS.type == "windows", 2L, 0L),
 	stop("Invalid case parameter: nochange | toupper | tolower | common db names")
	)
   case <- switch(case+1L, "nochange", "toupper", "tolower")
   rows_at_time <- max(1, min(1024, rows_at_time))
   cs <- attr(stat, "connection.string")
   if(grepl("PWD=", cs)) {
       attr(stat, "connection.string") <- sub("PWD=[^;]+($|;)", "PWD=******;", cs)
       Call$connection <- sub("PWD=[^;]+($|;)", "PWD=******;", connection)
   }
   structure(stat, class = "RODBC", case = case, id = id,
             believeNRows = believeNRows,
             colQuote = colQuote, tabQuote = tabQuote,
             interpretDot = interpretDot,
             encoding = DBMSencoding,
             rows_at_time = rows_at_time, isMySQL = isMySQL,
             call = Call)
}

odbcQuery <-
    function(channel, query, rows_at_time = attr(channel, "rows_at_time"))
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    if(nchar(enc <- attr(channel, "encoding"))) query <- iconv(query, to=enc)
    .Call(C_RODBCQuery, attr(channel, "handle_ptr"), as.character(query),
          as.integer(rows_at_time))
}

odbcUpdate <-
    function(channel, query, data, params, test = FALSE, verbose = FALSE,
             nastring = NULL)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    ## sanity checks!
    if(length(params) == 0L || nrow(params) == 0L)
        stop("no parameters, so nothing to update")
    if(nchar(enc <- attr(channel, "encoding"))) query <- iconv(query, to=enc)
    vflag <- 0
    if(verbose) vflag <- 1
    if(test) vflag <- 2
    ## apply the name mangling that was applied when the table was created
    cnames <- mangleColNames(names(data))
    cnames <- switch(attr(channel, "case"),
                     nochange = cnames,
                     toupper = toupper(cnames),
                     tolower = tolower(cnames))
    for(i in seq_along(data))
        if(!is.numeric(data[[i]])) {
            data[[i]] <- as.character(data[[i]])
            if(nchar(enc)) data[[i]] <- iconv(data[[i]], to = enc)
        }
    ## now map names: ds[i] is the data col to go with param i.
    ds <- match(params[[1]], cnames)
    if(any(is.na(ds))) stop("missing columns in 'data'")
    ## but pass 0-indexed version of ds
    .Call(C_RODBCUpdate, attr(channel, "handle_ptr"), as.character(query),
          data, ds-1L, params, as.integer(vflag))
}

## catalog, schema, tableName are 'pattern-value's
## IBM says % is special, and asks for all schemas or tableTypes to be listed.
## MSDN says there are values SQL_ALL_CATALOGS, SQL_ALL_SCHEMAS,
## SQL_ALL_TABLE_TYPES, but the headers define these as "%".
## tableType is a character vector containing one of more of
## "TABLE" "VIEW" "SYSTEM TABLE" "ALIAS" "SYNONYM" (may be single-quoted).
## http://publib.boulder.ibm.com/infocenter/dzichelp/v2r2/index.jsp?topic=/com.ibm.db29.doc.odbc/db2z_fntables.htm
odbcTables <- function(channel, catalog = NULL, schema = NULL,
                        tableName = NULL, tableType = NULL, literal = FALSE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    tableType  <- if(is.character(tableType) && length(tableType))
        paste(tableType, collapse=",") else NULL
    .Call(C_RODBCTables, attr(channel, "handle_ptr"),
          catalog, schema, tableName, tableType, as.logical(literal))
}

odbcColumns <- function(channel, table, catalog = NULL, schema = NULL,
                        literal = FALSE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    .Call(C_RODBCColumns, attr(channel, "handle_ptr"),
          as.character(table), catalog, schema, as.logical(literal))
}

odbcSpecialColumns <- function(channel, table, catalog = NULL, schema = NULL)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    .Call(C_RODBCSpecialColumns, attr(channel, "handle_ptr"),
          as.character(table), catalog, schema)
}

odbcPrimaryKeys <- function(channel, table, catalog = NULL, schema = NULL)
{
    if(!odbcValidChannel(channel))
        stop("first argument is not an open RODBC channel")
    .Call(C_RODBCPrimaryKeys, attr(channel, "handle_ptr"),
          as.character(table), catalog, schema)
}

close.RODBC <- function(con, ...) invisible(ifelse(odbcClose(con), 0L, 1L))

odbcClose <- function(channel)
{
    if(!odbcValidChannel(channel))
       stop("argument is not an open RODBC channel")
    res <- .Call(C_RODBCClose, attr(channel, "handle_ptr"))
    if(res > 0) invisible(FALSE) else {
        warning(paste(odbcGetErrMsg(channel), sep="\n"))
        FALSE
    }
    invisible(TRUE)
}

odbcCloseAll <- function()
{
    .Call(C_RODBCCloseAll)
    invisible()
}

odbcFetchRows <-
    function(channel, max = 0, buffsize = 1000,
             nullstring = NA_character_, believeNRows = TRUE)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    .Call(C_RODBCFetchRows, attr(channel, "handle_ptr"), max, buffsize,
          as.character(nullstring), believeNRows)
}

odbcCaseFlag <- function (channel)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    attr(channel, "case")
}

odbcGetInfo <- function(channel)
{
    if(!odbcValidChannel(channel))
       stop("argument is not an open RODBC channel")
    res <- .Call(C_RODBCGetInfo, attr(channel, "handle_ptr"))
    names(res) <- c("DBMS_Name", "DBMS_Ver", "Driver_ODBC_Ver",
                    "Data_Source_Name", "Driver_Name", "Driver_Ver",
                    "ODBC_Ver", "Server_Name")
    res
}

odbcValidChannel <- function(channel)
{
    inherits(channel, "RODBC") && is.integer(channel) &&
    .Call(C_RODBCcheckchannel, channel, attr(channel, "id")) > 0
}

odbcClearResults <-  function(channel)
{
    if(!odbcValidChannel(channel))
       stop("first argument is not an open RODBC channel")
    .Call(C_RODBCclearresults, attr(channel, "handle_ptr"))
    invisible()
}

print.RODBC <- function(x, ...)
{
    con <- strsplit(attr(x, "connection.string"), ";", fixed = TRUE)[[1L]]
    case <- paste("case=", attr(x, "case"), sep="")
    cat("RODBC Connection ", as.vector(x), "\nDetails:\n  ", sep = "")
    cat(case, con, sep="\n  ")
    invisible(x)
}

odbcSetAutoCommit <- function(channel, autoCommit = TRUE)
{
    if(!odbcValidChannel(channel))
         stop("first argument is not an open RODBC channel")
    .Call(C_RODBCSetAutoCommit, attr(channel, "handle_ptr"), autoCommit)
}

odbcEndTran <- function(channel, commit = TRUE)
{
    if(!odbcValidChannel(channel))
         stop("first argument is not an open RODBC channel")
    .Call(C_RODBCEndTran, attr(channel, "handle_ptr"), commit)
}

odbcDataSources <- function(type = c("all", "user", "system"))
{
    type <- match.arg(type)
    type <- match(type, c("all", "user", "system"))
    .Call(C_RODBCListDataSources, type)
}
