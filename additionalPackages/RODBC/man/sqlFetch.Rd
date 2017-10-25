% file RODBC/man/sqlFetch.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlFetch}
\alias{sqlFetch}
\alias{sqlFetchMore}

\title{Reading Tables from ODBC Databases}
\description{
  Read some or all of a table from an ODBC database into a data frame.
}
\usage{
sqlFetch(channel, sqtable, ..., colnames = FALSE, rownames = TRUE)

sqlFetchMore(channel, ..., colnames = FALSE, rownames = TRUE)
}
\arguments{
  \item{channel}{connection handle returned by \code{\link{odbcConnect}}.}
  \item{sqtable}{a database table name accessible from the connected DSN.
    This should be either a literal character string or a character vector of
    length 1.}
  \item{\dots}{additional arguments to be passed to
    \code{\link{sqlQuery}} or \code{\link{sqlGetResults}}.  See
    \sQuote{Details}.}
  \item{colnames}{logical: retrieve column names from first row of table?
    (For use when \code{\link{sqlSave}(colnames = TRUE)} was used.)}
  \item{rownames}{either logical or character.
    If logical, retrieve row names from the first column
    (\code{rownames}) in the table?  If character, the column name to
    retrieve them from.}
}
\details{
  Note the \sQuote{table} includes whatever table-like objects are
  provided by the DBMS, in particular views and system tables.
  
  \code{sqlFetch} by default retrieves the the entire contents of the table
  \code{sqtable}.  Rownames and column names are restored as indicated
  (assuming that they have been placed in the table by the corresponding
  arguments to \code{\link{sqlSave}}).

  Alternatively, \code{sqlFetch} can fetch the first \code{max} rows, in
  which case \code{sqlFetchMore} will retrieve further result rows,
  provided there has been no other ODBC query on that channel in the
  meantime.

  These functions try to cope with the peculiar way the Excel ODBC
  driver handles table names, and to quote Access table names which
  contain spaces.  Dotted table names, e.g. \code{myschema.mytable}, are
  allowed on systems that support them, unless the connection was opened
  with \code{interpretDot = FALSE}.

  Useful additional parameters to pass to \code{\link{sqlQuery}} or
  \code{\link{sqlGetResults}} include
  \describe{
    \item{\code{max}:}{limit on the number of rows to fetch, with
      \code{0} (the default) indicating no limit.}
    
    \item{\code{nullstring}:}{character string to be used when reading
      \code{SQL_NULL_DATA} character items from the database:
      default \code{NA_character_}.}
    
    \item{\code{na.strings}:}{character string(s) to be mapped to
      \code{NA} when reading character data: default \code{"NA"}.}
    
    \item{\code{as.is}:}{as in \code{\link{sqlGetResults}}.}
    
    \item{\code{dec}:}{The character for the decimal place to be assumed
      when converting character columns to numeric.}

    \item{\code{rows_at_time}:}{Allow for multiple rows to be retrieved at
      once.  See \code{\link{sqlQuery}}.}
  }
}
\value{
  A data frame on success, or a character or numeric error code (see
  \code{\link{sqlQuery}}).
}
\note{
  If the table name desired is not a valid SQL name (alphanumeric plus
  \code{_}) and these functions are not able to interpret the name, you
  can use \code{\link{sqlQuery}} with whatever quoting mechanism
  your DBMS vendor provides (e.g. \code{[ ]} on some Microsoft
  products and backticks on MySQL).
}
\seealso{
  \code{\link{sqlSave}}, \code{\link{sqlQuery}},
  \code{\link{odbcConnect}}, \code{\link{odbcGetInfo}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{
\dontrun{
channel <- odbcConnect("test")
sqlSave(channel, USArrests)
sqlFetch(channel, "USArrests") # get the lot
sqlFetch(channel, "USArrests", max = 20, rows_at_time = 10)
sqlFetchMore(channel, max = 20)
sqlFetchMore(channel) # get the rest
sqlDrop(channel, "USArrests") 
close(channel)
}}
\keyword{IO}
\keyword{database}
