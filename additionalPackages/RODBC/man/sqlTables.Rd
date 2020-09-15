% file RODBC/man/s.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlTables}
\alias{sqlTables}

\title{List Tables on an ODBC Connection}

\description{
  List the table-like objects accessible from an ODBC connection.  What
  objects are \sQuote{table-like} depends on the DBMS, ODBC driver and
  perhaps even the configuration settings: in particular some
  connections report system tables and some do not.
}
\usage{
sqlTables(channel, errors = FALSE, as.is = TRUE,
          catalog = NULL, schema = NULL, tableName = NULL,
          tableType = NULL, literal = FALSE)
}
\arguments{
  \item{channel}{connection handle as returned by
    \code{\link{odbcConnect}}.}
  \item{errors}{if \code{TRUE} halt and display error, else return \code{-1}.}
  \item{as.is}{as in \code{\link{sqlGetResults}}.}
  \item{catalog, schema, tableName, tableType}{\code{NULL} or character:
    whether these do anything depends on the ODBC driver.  The first
    three can be length-one character vectors, and \code{tableType} can
    specify zero or more types in separate elements of a character vector.}
  \item{literal}{logical: (where supported) should arguments be
  interpreted literally or including wildcards?}
}
\value{
  A data frame on success, or character/numeric on error depending on
  the \code{errors} argument. (Use \code{\link{sqlGetResults}} for
  further details of errors.)

  The column names depend on the database, containing a third column
  \code{TABLE_NAME} (not always in upper case): however, they are
  supposed to be always in the same order.
  
  The first column is the \sQuote{catalog} or (in ODBC2 parlance)
  \sQuote{qualifier}, the second the \sQuote{schema} or (ODBC2)
  \sQuote{owner}, the third the name, the fourth the table type (one of
  \code{"TABLE"}, \code{"VIEW"}, \code{"SYSTEM TABLE"}, \code{"ALIAS"},
  \code{"SYNONYM"}, or a driver-specific type name) and the fifth
  column any remarks.

  Oddly, the Microsoft Excel driver considers worksheets to be
  system tables, and named ranges to be tables.
}

\section{Driver-specific details}{
  Whether the additional arguments are implemented and what they do is
  driver-specific.  The standard SQL wildcards are \emph{underscore} to
  match a single character and \emph{percent} to match zero or more
  characters (and often backslash will escape these): these are not used
  for table types.  All of these drivers interpret wildcards in
  \code{tableName}, and in \code{catalog} or \code{schema} where
  supported.
  
  Setting one of \code{catalog} or \code{schema} to \code{"\%"} and the
  other and \code{tableName} to \code{""} should give a list of
  available catalogs or schemas, whereas
  \preformatted{
  catalog = "", schema = "", tableName = "", tableType = "\%"
  }
  should list the supported table types.
  
  For MySQL, \code{catalog} refers to a database whereas
  \code{schema} is mostly ignored, and \code{literal} is ignored.  To
  list all databases use just \code{catalog = "\%"}.  In the 5.1.x
  driver, use \code{catalog="\var{db_name}", tableName="\%"} to list the
  tables in another database, and to list the table types use the
  form displayed above.

  For PostgreSQL's ODBC driver \code{catalog} is ignored (except that
  \code{catalog = ""} is required when listing schema or table types) and
  \code{literal} works for both \code{schema} and for \code{tableName}.

  SQLite ODBC ignores \code{catalog} and \code{schema}, except that the
  displayed form is used to list table types.  So although it is
  possible to attach databases and to refer to them by the \emph{dotted
  name} notation, it is apparently impossible to list tables on attached
  databases.
  % wildcards in \code{tableName} are used whatever the setting of \code{literal}.

  Microsoft SQL Server 2008 interprets both \code{catalog} and \code{schema}.
  With \code{literal = TRUE} it only finds tables if \code{schema} is
  set (even to an empty string).  Schemas are only listed if they contain
  objects.

  Oracle's Windows ODBC driver finds no matches if anything non-empty is
  supplied for the \code{catalog} argument.  Unless a schema is
  specified it lists tables in all schemas.  It lists available table
  types as just \code{"TABLE"} and \code{"VIEW"}, but other types appear
  in listings.  With \code{literal = TRUE} it only finds tables if
  \code{schema} is set (even to an empty string). 

  DB2 implements schemas but not catalogs.  \code{literal = TRUE} has no
  effect.  In some uses case matters and upper-case names must be used
  for schemas.
  
  The Microsoft Access and Excel drivers interpret \code{catalog} as the
  name of the Access \code{.mdb} or Excel \code{.xls} file (with the
  path but without the extension): wildcards are interpreted in
  \code{catalog} (for files in the same folder as the attached database)
  and \code{tableName}.  Using \code{schema} is an error except when
  listing catalogs or table types.  The Excel driver matched
  \code{tableType = "TABLE"} (a named range) but not \code{tableType =
    "SYSTEM TABLE"} (the type returned for worksheets).

  The Actual Technologies Access/Excel driver ignores all the additional
  arguments.
}
\seealso{
  \code{\link{sqlGetResults}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{
\dontrun{
> sqlTables(channel, "USArrests")
## MySQL example
  TABLE_CAT TABLE_SCHEM TABLE_NAME TABLE_TYPE REMARKS
1    ripley              USArrests      TABLE        
## PostgreSQL example
  TABLE_QUALIFIER TABLE_OWNER TABLE_NAME TABLE_TYPE REMARKS
1          ripley      public  usarrests      TABLE        
## Microsoft Access example
> sqlTables(channel)
      TABLE_CAT TABLE_SCHEM        TABLE_NAME   TABLE_TYPE REMARKS
1 C:\\bdr\\test        <NA> MSysAccessObjects SYSTEM TABLE    <NA>
2 C:\\bdr\\test        <NA>          MSysACEs SYSTEM TABLE    <NA>
3 C:\\bdr\\test        <NA>       MSysObjects SYSTEM TABLE    <NA>
4 C:\\bdr\\test        <NA>       MSysQueries SYSTEM TABLE    <NA>
5 C:\\bdr\\test        <NA> MSysRelationships SYSTEM TABLE    <NA>
6 C:\\bdr\\test        <NA>             hills        TABLE    <NA>
7 C:\\bdr\\test        <NA>         USArrests        TABLE    <NA>
}}
\keyword{IO}
\keyword{database}
