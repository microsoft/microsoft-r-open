% file RODBC/man/sqlDrop.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlDrop}
\alias{sqlDrop}
\alias{sqlClear}

\title{Deletion Operations on Tables in ODBC databases}
\description{
  \code{sqlClear} deletes all the rows of the table \code{sqtable}.

  \code{sqlDrop} removes the table \code{sqtable} (if permitted).
}
\usage{
sqlClear(channel, sqtable, errors = TRUE)

sqlDrop(channel, sqtable, errors = TRUE)
}
\arguments{
  \item{channel}{connection object as returned by \code{\link{odbcConnect}}.}
  \item{sqtable}{character string: a database table name accessible from the
    connected DSN.  This can be a \sQuote{dotted} name of the form
    \code{\var{schema}.\var{table}}.}
  \item{errors}{logical: if \code{TRUE} halt and display error, else
    return \code{-1}.}
}
\details{
  These submit \samp{TRUNCATE TABLE} and \samp{DROP TABLE} SQL queries
  respectively.

  \sQuote{Dotted} table names are allowed on systems that support them
  but the existence of the table is not checked and so attempting these
  operations on a non-existent table will give a low-level error.  (This
  can be suppressed by opening the connection with \code{interpretDot =
    FALSE}.)

  The default \sQuote{drop} behaviour in Oracle is to move the table to
  the \sQuote{recycle bin}: use
\preformatted{
  sqlQuery(channel, "PURGE recyclebin")
}
  to empty the recycle bin.

  The current user might not have privileges to allow these operations,
  and Actual Technologies' Mac OS X SQLite driver has a bug causing them
  silently to fail.
}  

\value{
  If \code{errors = FALSE}, a numeric value, invisibly.
  Otherwise a character string or \code{invisible()}.
}
\seealso{
  \code{\link{odbcConnect}}, \code{\link{sqlQuery}}, \code{\link{sqlFetch}},
  \code{\link{sqlSave}}, \code{\link{sqlTables}}, \code{\link{odbcGetInfo}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\keyword{IO}
\keyword{database}
