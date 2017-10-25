% file RODBC/man/sqlColumns.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlColumns}
\alias{sqlPrimaryKeys}
\alias{sqlColumns}

\title{Query Column Structure in ODBC Tables}
\description{
  Enquire about the column structure of tables on an ODBC database
  connection.
}
\usage{
sqlColumns(channel, sqtable, errors = FALSE, as.is = TRUE,
           special = FALSE, catalog = NULL, schema = NULL,
           literal = FALSE)

sqlPrimaryKeys(channel, sqtable, errors = FALSE, as.is = TRUE,
               catalog = NULL, schema = NULL)
}
\arguments{
  \item{channel}{connection object as returned by \code{\link{odbcConnect}}.}
  \item{sqtable}{character string: a database table (or view or similar)
    name accessible from the connected DSN.  If wildcards are allowed
    (only for \code{sqlColumns(special=FALSE)}),
    results for all matching tables.}
  \item{errors}{logical: if true halt and display error, else return \code{-1}.}
  \item{as.is}{see \code{\link{sqlGetResults}}.}
  \item{special}{logical.  If true, return only the column(s) needed to
    specify a row uniquely.  Depending on the database, there might be none.} 
  \item{catalog, schema}{\code{NULL} or character: additional
    information on where to locate the table: see
    \code{\link{sqlTables}} for driver-specific details.  Wildcards may
    be supported in \code{schema} for \code{sqlColumns(special=FALSE)}.}
  \item{literal}{logical: wildcards may be interpreted in
    \code{schema} and \code{sqtable}: if so this may
    suppress such interpretation.}
}
\details{
  The argument \code{special = TRUE} to \code{sqlColumns} returns the
  column(s) needed to specify a row uniquely.  This is intended to form
  the basis of an SQL \code{WHERE} clause for update queries (see
  \code{\link{sqlUpdate}}), and what (if anything) it does is
  DBMS-specific.  On many DBMSs it will return the primary keys if
  present: on others it will return a pseudo-column such as \samp{ROWID}
  (Oracle) or \samp{_ROWID_} (SQLite), either always (Oracle) or if
  there is no primary key.

  Primary keys are implemented in some DBMSs and drivers.  A table can
  have a single column designated as a primary key or, in some cases,
  multiple columns.  Primary keys should not be nullable (that is,
  cannot contain missing values).  They can be specified as part of a
  \samp{CREATE TABLE} statement or added by a \samp{ALTER TABLE}
  statement.

  In principle specifying \code{catalog} should select an alternative
  database in MySQL or an attached database in SQLite, but neither works
  with current drivers.

  If \code{sqtable} contains \samp{.} and neither \code{catalog} nor
  \code{schema} is supplied, an attempt is made to interpret
  \code{\var{qualifier}.\var{table}} as table \code{\var{table}} in
  schema \code{\var{qualifier}} (and for MySQL \sQuote{schema} means
  \sQuote{database}, but the current drivers fail to interpret
  \code{catalog=}, so this does not yet work). (This can be suppressed
  by opening the connection with \code{interpretDot = FALSE}.)  This has
  been tested successfully on PostgreSQL, SQL Server, Oracle, DB2 and
  Mimer.
  
  Whether wildcards are accepted for \code{sqtable} and \code{schema} in
  \code{sqlColumns(special = FALSE)} depends on the driver and may be
  changed by the value of \code{literal}.  For example, the PostgreSQL
  driver tested allowed wildcards in \code{schema} only if \code{literal
  = FALSE} and never in \code{sqtable}, whereas two MySQL drivers both
  failed to match a database when \code{catalog} was supplied and always
  allowed wildcards in \code{sqtable} even if \code{literal = TRUE}.
  % SQLiteODBC allows wildcards whatever \code{literal} is.
  % SQL Server and Oracle find nothing if literal = TRUE, and does
  % not access attached databases.
}
\value{  
  A data frame on success.  If no data is returned, either a
  zero-row data frame or an error. (For example, if there are no primary
  keys or special column(s) in this table an empty data frame is
  returned, but if primary keys are not supported by the ODBC driver or
  DBMS, an error code results.)

  The column names are not constant across ODBC versions so the
  data should be accessed by column number.

  For \code{sqlPrimaryKeys} and \code{sqlColumns(special=FALSE)} the
  first four columns give the catalog, schema, table and column names
  (where applicable).  For \code{sqlPrimaryKeys} the next two columns
  are the column sequence number (starting with 1) and name of the
  primary key: drivers can define further columns.  For
  \code{sqlColumns(special=FALSE)} there are 18 columnns: see
  \url{http://msdn.microsoft.com/en-us/library/ms711683(VS.85).aspx}.
  Those beyond the first 6 shown in the examples give the
  \sQuote{ordinal position} (column 17) and further characteristics of the
  column type: see \code{\link{sqlTypeInfo}}.
    
  For the numeric values returned by \code{sqlColumns(special=TRUE)} see
  \url{http://msdn.microsoft.com/en-us/library/ms714602(VS.85).aspx}:
  the scope should always be \code{2} (the session) since that is the
  scope requested in the call. For the \code{PSEUDO_COLUMN} column, the
  possible values are \code{0} (unknown), \code{1} (no) and \code{2}
  (yes).
}
\seealso{
  \code{\link{odbcConnect}}, \code{\link{sqlQuery}}, \code{\link{sqlFetch}},
  \code{\link{sqlSave}}, \code{\link{sqlTables}}, \code{\link{odbcGetInfo}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{
\dontrun{## example results from MySQL
> channel <- odbcConnect("test")
> sqlDrop(channel, "USArrests", errors = FALSE) # precautionary
> sqlSave(channel, USArrests, addPK = TRUE)
> sqlColumns(channel, "USArrests")
  TABLE_CAT TABLE_SCHEM TABLE_NAME COLUMN_NAME DATA_TYPE TYPE_NAME
1    ripley        <NA>  USArrests    rownames        12   varchar
2    ripley        <NA>  USArrests      Murder         8    double
3    ripley        <NA>  USArrests     Assault         4   integer
4    ripley        <NA>  USArrests    UrbanPop         4   integer
5    ripley        <NA>  USArrests        Rape         8    double
... 12 more columns

> sqlColumns(channel, "USArrests", special = TRUE)
  SCOPE COLUMN_NAME DATA_TYPE TYPE_NAME COLUMN_SIZE BUFFER_LENGTH
1     2    rownames        12   varchar         255           255
  DECIMAL_DIGITS PSEUDO_COLUMN
1             NA             1

> sqlPrimaryKeys(channel, "USArrests")
  TABLE_CAT TABLE_SCHEM TABLE_NAME COLUMN_NAME KEY_SEQ PK_NAME
1      <NA>        <NA>  USArrests    rownames       1 PRIMARY
> sqlDrop(channel, "USArrests")
> close(channel)
}}
\keyword{IO}
\keyword{database}
