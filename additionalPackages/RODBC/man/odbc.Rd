% file RODBC/man/odbc.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{odbc-low-level}
\alias{odbcQuery}
\alias{odbcTables}
\alias{odbcFetchRows}
\alias{odbcGetErrMsg}
\alias{odbcClearError}

\title{Low-level ODBC functions}
\description{
  \R functions which talk directly to the ODBC interface.
}
\usage{
odbcTables(channel, catalog = NULL, schema = NULL,
           tableName = NULL, tableType = NULL, literal = FALSE)

odbcQuery(channel, query, rows_at_time = attr(channel, "rows_at_time"))
odbcFetchRows(channel, max = 0, buffsize = 1000,
              nullstring = NA_character_, believeNRows = TRUE)

odbcGetErrMsg(channel)
odbcClearError(channel)
}
\arguments{
  \item{channel}{connection handle as returned by
    \code{\link{odbcConnect}}, of class \code{"RODBC"}.}
  \item{catalog, schema, tableName, tableType}{\code{NULL} or character:
    whether these do anything depends on the ODBC driver.  The first
    three can be length-one character vectors, and \code{tableType} can
    specify zero or more types.}
  \item{literal}{logical: should arguments be interpreted literally or
    including wildcards?}
  \item{query}{any valid SQL statement.}
  \item{rows_at_time}{The number of rows to fetch at a time, between 1
    and 1024.  Not all drivers work correctly with values > 1: see
    \code{\link{sqlQuery}}.}
  \item{max}{limit on the number of rows to fetch, with 0 indicating no limit.}
  \item{buffsize}{the number of records to be transferred at a time.}
  \item{nullstring}{character string to be used when reading
    \code{SQL_NULL_DATA} items in a column transferred as character.}
  \item{believeNRows}{logical.  Is the number of rows returned by the
    ODBC connection believable?}
}
\details{
  These are low-level functions called by \code{\link{sqlTables}},
  \code{\link{sqlQuery}}, \code{\link{sqlGetResults}} and similar
  high-level functions.  They are likely to be confind to the 
  \pkg{RODBC} namespace in the near future.
  
  \code{odbcTables} enquires about the tables on the connected database.
  Whether arguments after the first do anything and what they do
  depends on the ODBC driver: see the help on \code{\link{sqlTables}}
  for some driver-specific details.

  \code{odbcFetchRows} returns a data frame of the pending rowset,
  limited to \code{max} rows if \code{max} is greater than 0.

  \code{buffsize} may be increased from the default of 1000 rows for
  increased performance on a large dataset.  This only has an effect
  when \code{max = 0} and \code{believeNRows = FALSE} (either for the
  ODBC connection or for this function call), in which case
  \code{buffsize} is used as the initial allocation length of the \R
  vectors to hold the results.  (Values of less than 100 are increased
  to 100.)  If the initial size is too small the vector length is
  doubled, repeatedly if necessary.
}
\value{
  \code{odbcGetErrMsg} returns a (possibly zero-length)
  character vector of pending messages.

  \code{odbcClearError} returns nothing, invisibly.

  The otheres return \code{1} on success and \code{-1} on failure,
  indicating that a message is waiting to be retrieved
  \code{\link{odbcGetErrMsg}}.  \code{odbcFetchRows} may return
  \code{-2} indicating \code{"No Data"}, the message that would be
  returned by \code{\link{odbcGetErrMsg}}.
}
\seealso{
  \code{\link{sqlQuery}}, \code{\link{odbcConnect}},
  \code{\link{odbcGetErrMsg}}.
}
\author{
  Michael Lapsley and Brian Ripley
}
\keyword{IO}
\keyword{database}
