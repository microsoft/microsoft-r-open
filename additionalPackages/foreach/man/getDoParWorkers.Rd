\name{getDoParWorkers}
\alias{getDoParWorkers}
\alias{getDoParRegistered}
\alias{getDoParName}
\alias{getDoParVersion}
\title{Functions Providing Information on the doPar Backend}
\description{
The \code{getDoParWorkers} function returns the number of
execution workers there are in the currently registered doPar backend.
It can be useful when determining how to split up the work to be executed
in parallel.  A \code{1} is returned by default.

The \code{getDoParRegistered} function returns TRUE if a doPar backend
has been registered, otherwise FALSE.

The \code{getDoParName} function returns the name of the currently
registered doPar backend.  A \code{NULL} is returned if no backend is
registered.

The \code{getDoParVersion} function returns the version of the currently
registered doPar backend.  A \code{NULL} is returned if no backend is 
registered.
}
\usage{
getDoParWorkers()
getDoParRegistered()
getDoParName()
getDoParVersion()
}

\examples{
cat(sprintf('\%s backend is registered\n',
            if(getDoParRegistered()) 'A' else 'No'))
cat(sprintf('Running with \%d worker(s)\n', getDoParWorkers()))
(name <- getDoParName())
(ver <- getDoParVersion())
if (getDoParRegistered())
  cat(sprintf('Currently using \%s [\%s]\n', name, ver))
}

\keyword{utilities}
