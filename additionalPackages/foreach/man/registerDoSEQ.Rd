\name{registerDoSEQ}
\alias{registerDoSEQ}
\title{registerDoSEQ}
\description{
The \code{registerDoSEQ} function is used to explicitly register
a sequential parallel backend with the foreach package.
This will prevent a warning message from being issued if the
\code{\%dopar\%} function is called and no parallel backend has
been registered.
}
\usage{
registerDoSEQ()
}
\seealso{
  \code{\link[doSNOW]{registerDoSNOW}}
}

\examples{
# specify that \%dopar\% should run sequentially
registerDoSEQ()
}

\keyword{utilities}
