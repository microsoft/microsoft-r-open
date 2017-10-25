% file RODBC/man/sqlSave.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlSave}
\alias{sqlSave}
\alias{sqlUpdate}

\title{Write a Data Frame to a Table in an ODBC Database}
\description{
  Write or update a table in an ODBC database.
}
\usage{
sqlSave(channel, dat, tablename = NULL, append = FALSE,
        rownames = TRUE, colnames = FALSE, verbose = FALSE,
        safer = TRUE, addPK = FALSE, typeInfo, varTypes,
        fast = TRUE, test = FALSE, nastring = NULL)

sqlUpdate(channel, dat, tablename = NULL, index = NULL,
          verbose = FALSE, test = FALSE, nastring = NULL,
          fast = TRUE)
}
\arguments{
  \item{channel}{connection handle returned by \code{\link{odbcConnect}}.}
  \item{dat}{a data frame.}
  \item{tablename}{character: a database table name accessible from
    the connected DSN.  If missing, the name of \code{dat}.}
  \item{index}{character.  Name(s) of index column(s) to be used.}
  \item{append}{logical.  Should data be appended to an existing table?}
  \item{rownames}{either logical or character.  If logical, save the row
    names as the first column \code{rownames} in the table?  If
    character, the column name under which to save the rownames.}
  \item{colnames}{logical: save column names as the first row of table?}
  \item{verbose}{display statements as they are sent to the server?}
  \item{safer}{logical. If true, create a non-existing table but only
    allow appends to an existing table. If false, allow \code{sqlSave}
    to attempt to delete all the rows of an existing table, or to drop it.}
  \item{addPK}{logical.  Should rownames (if included) be specified as a
    primary key?}
  \item{typeInfo}{optional list of DBMS datatypes.  Should have elements
    named \code{"character"}, \code{"double"} and \code{"integer"}.}
  \item{varTypes}{an optional named character vector giving the DBMSs
    datatypes to be used for some (or all) of the columns if a table is
    to be created.}
  \item{fast}{logical.  If false, write data a row at a time.  If true,
    use a parametrized \code{INSERT INTO} or \code{UPDATE} query to
    write all the data in one operation.}
  \item{test}{logical: if \code{TRUE} show what would be done, only.}
  \item{nastring}{optional character string to be used for writing
    \code{NA}s to the database.  See \sQuote{Details}.}
}
\details{
  \code{sqlSave} saves the data frame \code{dat} in the table
  \code{tablename}.  If the table exists and has the appropriate
  structure it is used, or else it is created anew.  If a new table is
  created, column names are remapped by removing any characters which
  are not alphanumeric or \code{_}, and the types are selected by
  consulting arguments \code{varTypes} and \code{typeInfo}, then looking
  the driver up in the database used by \code{\link{getSqlTypeInfo}} or
  failing that by interrogating \code{\link{sqlTypeInfo}}.

  If \code{rownames = TRUE} the first column of the table will be the
  row labels with colname \code{rowname}: \code{rownames} can also be a
  string giving the desired column name (see \sQuote{Examples}). If
  \code{colnames} is true, the column names are copied into row 1.  This
  is intended for cases where case conversion alters the original column
  names and it is desired that they are retained.  Note that there are
  drawbacks to this approach: it presupposes that the rows will be
  returned in the correct order; not always valid.  It will also cause
  numeric columns to be returned as factors.

  Argument \code{addPK = TRUE} causes the row names to be marked as a
  primary key.  This is usually a good idea, and may allow database
  updates to be done.  However, the ODBC drivers for some DBMSs
  (e.g. Access) do not support primary keys, and earlier versions of the
  PostgreSQL ODBC driver generated internal memory corruption if this
  option is used.
  
  \code{sqlUpdate} updates the table where the rows already exist.  Data
  frame \code{dat} should contain columns with names that map to (some
  of) the columns in the table.  It also needs to contain the column(s)
  specified by \code{index} which together identify the rows to be
  updated.  If \code{index = NULL}, the function tries to identify such
  columns.  First it looks for a primary key for the table, then for the
  column(s) that the database regards as the optimal for defining a row
  uniquely (these are returned by \code{\link{sqlColumns}(special =
  TRUE)}: if this returns a pseudo-column it cannot be used as we do not
  have values for the rows to be changed).  Finally, the row names are
  used if they are stored as column \code{"rownames"} in the table.
  
  When \code{fast = TRUE}, \code{NA}s are always written as SQL nulls in
  the database, and this is also the case if \code{fast = FALSE} and
  \code{nastring = NULL} (its default value).  Otherwise \code{nastring}
  gives the character string to be sent to the driver when \code{NA}s
  are encountered: for all but the simplest applications it will be
  better to prepare a data frame with non-null missing values already
  substituted.

  If \code{fast = FALSE} all data are sent as character strings.
  If \code{fast = TRUE}, integer and double vectors are sent as types
  \code{SQL_C_SLONG} and \code{SQL_C_DOUBLE} respectively.  Some drivers
  seem to require \code{fast = FALSE} to send other types,
  e.g. \code{datetime}.  SQLite's approach is to use the data to determine
  how it is stored, and this does not work well with \code{fast = TRUE}.

  If \code{tablename} contains \samp{.} and neither \code{catalog} nor
  \code{schema} is supplied, an attempt is made to interpret
  \code{\var{qualifier}.\var{table}} names as table \code{\var{table}}
  in schema \code{\var{qualifier}} (and for MySQL \sQuote{schema} means
  \sQuote{database}).  (This can be suppressed by opening the connection with
  \code{interpretDot = FALSE}.)
}
\section{Warning}{
  \code{sqlSave(safer = FALSE)} uses the \sQuote{great white shark}
  method of testing tables (bite it and see).  The logic will
  unceremoniously \code{DROP} the table and create it anew with its own
  choice of column types in its attempt to find a writable
  solution. \code{test = TRUE} will not necessarily predict this
  behaviour. Attempting to write indexed columns or writing to
  pseudo-columns are less obvious causes of failed writes followed by a
  \code{DROP}.  If your table structure is precious it is up to you back
  it up.
}
\value{
  \code{1} invisibly for success (and failures cause errors).
}
\seealso{
  \code{\link{sqlFetch}}, \code{\link{sqlQuery}},
  \code{\link{odbcConnect}}, \code{\link{odbcGetInfo}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{
\dontrun{
channel <- odbcConnect("test")
sqlSave(channel, USArrests, rownames = "state", addPK=TRUE)
sqlFetch(channel, "USArrests", rownames = "state") # get the lot
foo <- cbind(state=row.names(USArrests), USArrests)[1:3, c(1,3)]
foo[1,2] <- 222
sqlUpdate(channel, foo, "USArrests")
sqlFetch(channel, "USArrests", rownames = "state", max = 5)
sqlDrop(channel, "USArrests") 
close(channel)
}}
\keyword{IO}
\keyword{database}
