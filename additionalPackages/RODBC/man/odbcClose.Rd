% file RODBC/man/odbcClose.Rd
% copyright (C) 1999-2002  M. Lapsley
% copyright (C) 2002-2013  B. D. Ripley
%
\name{odbcClose}
\alias{odbcClose}
\alias{odbcCloseAll}
\alias{close.RODBC}
\title{ODBC Close Connections}
\description{
  Close connections to ODBC databases.
}
\usage{
odbcClose(channel)

\method{close}{RODBC}(con, \dots)

odbcCloseAll()
}
\arguments{
  \item{channel, con}{RODBC connection object as returned by
    \code{odbcConnect}.}
  \item{\dots}{additional arguments passed from the generic.}
}
\details{
  \code{odbcClose} cleans up and frees resources.  It is also the method for the
  generic function \code{close}.

  \code{odbcCloseAll} closes all open channels (amongst the first 1000
  used in the session).

  Channels are closed at the end of an \R session, and may also be
  closed by garbage collection if no object refers to them.  In both
  cases a warning is given (but may not be seen at the end of a console
  session).
}
\value{
  Function \code{odbcClose} returns a logical indicating if it
  succeeded, invisibly unless a warning is given.

  The \code{close} method returns \code{0} (success) or \code{1}, invisibly.
}
\author{
  Michael Lapsley and Brian Ripley
}
\seealso{
  \code{\link{odbcConnect}}
}
\keyword{IO}
\keyword{database}
