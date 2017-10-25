% file RODBC/man/sqlCopy.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{sqlCopy}
\alias{sqlCopy}
\alias{sqlCopyTable}

\title{ODBC Copy}
\description{
  Functions to copy tables or result sets from one database to another.
}
\usage{
sqlCopy(channel, query, destination, destchannel = channel,
        verbose = FALSE, errors = TRUE, \dots)

sqlCopyTable(channel, srctable, desttable, destchannel = channel,
             verbose = FALSE, errors = TRUE)
}
\arguments{
  \item{channel, destchannel}{connection handle as returned by
    \code{\link{odbcConnect}}.}
  \item{query}{any valid SQL statement}
  \item{destination, srctable, desttable}{character: a
    database table name accessible from the connected DSN.}
  \item{verbose}{Display statements as they are sent to the server?}
  \item{errors}{if TRUE halt and display error, else return \code{-1}.}
  \item{\dots}{additional arguments to be passed to \code{\link{sqlSave}}.}
}
\details{
  \code{sqlCopy} as is like \code{\link{sqlQuery}}, but saves the output
  of \code{query} in table \code{destination} on channel
  \code{destchannel}.     
  
  \code{sqlCopyTable} copies the structure of \code{srctable} to
  \code{desttable} on DSN \code{destchannel}.  This is within the
  limitations of the ODBC lowest common denominator.  More precise
  control is possible \emph{via} \code{sqlQuery}.
}
\value{
  See \code{\link{sqlGetResults}}.
}
\seealso{
  \code{\link{sqlQuery}}, \code{\link{sqlSave}}
}
\author{
  Michael Lapsley and Brian Ripley
}
\examples{\dontrun{## example for a l/casing DBMS
sqlSave(channel, USArrests, rownames = "state")
query <- paste("select state, murder from usarrests",
               "where rape > 30", "order by murder")
sqlCopy(channel, query, "HighRape", rownames = FALSE)
sqlFetch(channel, "HighRape", rownames = "state", max = 5)
sqlDrop(channel, "HighRape")
}}
\keyword{IO}
\keyword{database}
