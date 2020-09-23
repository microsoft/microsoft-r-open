\name{getDoSeqWorkers}
\alias{getDoSeqWorkers}
\alias{getDoSeqRegistered}
\alias{getDoSeqName}
\alias{getDoSeqVersion}
\title{Functions Providing Information on the doSeq Backend}
\description{
The \code{getDoSeqWorkers} function returns the number of
execution workers there are in the currently registered doSeq backend.
A \code{1} is returned by default.

The \code{getDoSeqRegistered} function returns TRUE if a doSeq backend
has been registered, otherwise FALSE.

The \code{getDoSeqName} function returns the name of the currently
registered doSeq backend.  A \code{NULL} is returned if no backend is
registered.

The \code{getDoSeqVersion} function returns the version of the currently
registered doSeq backend.  A \code{NULL} is returned if no backend is 
registered.
}
\usage{
getDoSeqWorkers()
getDoSeqRegistered()
getDoSeqName()
getDoSeqVersion()
}

\examples{
cat(sprintf('\%s backend is registered\n',
            if(getDoSeqRegistered()) 'A' else 'No'))
cat(sprintf('Running with \%d worker(s)\n', getDoSeqWorkers()))
(name <- getDoSeqName())
(ver <- getDoSeqVersion())
if (getDoSeqRegistered())
  cat(sprintf('Currently using \%s [\%s]\n', name, ver))
}

\keyword{utilities}
