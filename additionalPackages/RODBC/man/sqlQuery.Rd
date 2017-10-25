% file RODBC/man/sqlQuery.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlQuery}
\alias{sqlQuery}
\alias{sqlGetResults}

\title{Query an ODBC Database}
\description{
  Submit an SQL query to an ODBC database, and retrieve the results.
}
\usage{
sqlQuery(channel, query, errors = TRUE, ..., rows_at_time)

sqlGetResults(channel, as.is = FALSE,  errors = FALSE,
              max = 0, buffsize = 1000,
              nullstring = NA_character_, na.strings = "NA",
              believeNRows = TRUE, dec = getOption("dec"),
              stringsAsFactors = default.stringsAsFactors())
}
\arguments{
  \item{channel}{connection handle as returned by \code{\link{odbcConnect}}.}
  \item{query}{any valid SQL statement.}
  \item{errors}{logical: if true halt and display error, else return \code{-1}.}
  \item{\dots}{additional arguments to be passed to \code{sqlGetResults}.}
  \item{rows_at_time}{The number of rows to fetch at a time, between 1
    and 1024.  See \sQuote{Details}.}
  \item{as.is}{which (if any) columns returned as character should be
    converted to another type?  Allowed values are as for
    \code{\link{read.table}}.  See \sQuote{Details}.}
  \item{max}{limit on the number of rows to fetch, with \code{0}
    indicating no limit.}
  \item{buffsize}{an initial guess at the number of rows, used if
    \code{max = 0} and \code{believeNRows == FALSE}.}
  \item{nullstring}{character string to be used when reading
    \code{SQL_NULL_DATA} character items from the database.}
  \item{na.strings}{character vector of strings to be mapped to
    \code{NA} when reading character data.}
  \item{believeNRows}{logical.  Is the number of rows returned by the
    ODBC connection believable?  This might have been set to false when
    the channel was opened, and if so that setting cannot be overridden.}
  \item{dec}{The character for the decimal place to be assumed when
    converting character columns to numeric.}
  \item{stringsAsFactors}{logical: should columns returned as character and not
    excluded by \code{as.is} and not converted to anything else be
    converted to factors?}
}
\details{
  \code{sqlQuery} is the workhorse function of \pkg{RODBC}.  It sends the SQL
  statement \code{query} to the server, using connection \code{channel}
  returned by \code{\link{odbcConnect}}, and retrieves (some or all of)
  the results \emph{via} \code{sqlGetResults}.

  The term \sQuote{query} includes any valid SQL statement including
  table creation, alteration, updates etc as well as \samp{SELECT}s.  The
  \code{sqlQuery} command is a convenience wrapper that first calls
  \code{\link{odbcQuery}} and then \code{sqlGetResults}. If
  finer-grained control is needed, for example over the number of rows
  fetched, additional arguments can be passed to \code{sqlQuery} or the
  underlying functions called directly.

  \code{sqlGetResults} is a mid-level function.  It is called after a
  call to \code{sqlQuery} or \code{\link{odbcQuery}} to retrieve waiting
  results into a data frame.  Its main use is with \code{max} set to
  non-zero when it will retrieve the result set in batches with repeated
  calls.  This is useful for very large result sets which can be
  subjected to intermediate processing.

  Where possible \code{sqlGetResults} transfers data in binary form:
  this happens for columns of (ODBC) SQL types \code{double},
  \code{real}, \code{integer} and \code{smallint}, and for binary SQL
  types (which are transferred as lists of raw vectors, given class
  \code{"ODBC_binary"}).  All other SQL data types are converted to
  character strings by the ODBC interface.
  
  This paragraph applies only to SQL data types which are returned by
  ODBC as character vectors.  If when creating the connection (see
  \code{\link{odbcConnect})} \code{DBMSencoding} was set to a non-empty
  value, the character strings are re-encoded.  Then if \code{as.is}
  is true for a column, it is returned as a character vector.  Otherwise
  (where detected) \code{date}, \code{datetime} and \code{timestamp}
  values are converted to the \code{"Date"} or \code{"POSIXct"} class.
  (Some drivers seem to confuse times with dates, so times may get
  converted too.  Also, some DBMSs (e.g. Oracle's) idea of \code{date}
  is a date-time.)  Remaining cases are converted by \R using
  \code{\link{type.convert}}.  When character data are to be converted
  to numeric data, the setting of \code{options("dec")} is used to map
  the character used by the ODBC driver in setting decimal points---this
  is set to a locale-specific value when \pkg{RODBC} is initialized if
  it is not already set.
  
  Using \code{buffsize} will yield a marginal increase in speed if set
  to no less than the maximum number of rows when \code{believeNRows =
    FALSE}.  (If set too small it can result in unnecessarily high
  memory use as the buffers will need to be expanded.)

  Modern drivers should work (and work faster, especially if
  communicating with a remote machine) with \code{rows_at_time = 100},
  the usual default, or more.  (However, some drivers may mis-fetch
  multiple rows, in which case set \code{rows_at_time = 1} when creating
  the connection.)  However, if \code{max} is specified then this
  may fetch too many rows and hence it could be reduced (but then this
  setting applies to all subsequent fetches from that result set).
  Another circumstance in which you might want to reduce
  \code{rows_at_time} is if there are large character columns in the
  result set: with the default value up to 6Mb of buffer for each such
  column could be allocated to store intermediate results.
}
\value{
  On success, a data frame (possibly with 0 rows) or character string.
  On error, if \code{errors = TRUE} a character vector of error
  message(s), otherwise an invisible integer error code \code{-1}
  (general, call \code{\link{odbcGetErrMsg}} for details) or \code{-2}
  (no data, which may not be an error as some SQL statements do return
  no data).
}
\seealso{
  \code{\link{odbcConnect}}, \code{\link{sqlFetch}},
  \code{\link{sqlSave}}, \code{\link{sqlTables}}, \code{\link{odbcQuery}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{
\dontrun{
channel <- odbcConnect("test")
sqlSave(channel, USArrests, rownames = "State", verbose = TRUE)
# options(dec=".") # optional, if DBMS is not locale-aware or set to ASCII
## note case of State, Murder, Rape are DBMS-dependent,
## and some drivers need column and table names double-quoted.
sqlQuery(channel, paste("select State, Murder from USArrests",
                        "where Rape > 30 order by Murder"))
close(channel)
}}
\keyword{IO}
\keyword{database}
