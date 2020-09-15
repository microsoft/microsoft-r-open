\name{setDoSeq}
\alias{setDoSeq}
\title{setDoSeq}
\description{
The \code{setDoSeq} function is used to register a sequential backend with the
foreach package.  This isn't normally executed by the user.  Instead, packages
that provide a sequential backend provide a function named \code{registerDoSeq}
that calls \code{setDoSeq} using the appropriate arguments.
}
\usage{
setDoSeq(fun, data=NULL, info=function(data, item) NULL)
}
\arguments{
  \item{fun}{A function that implements the functionality of \code{\%dopar\%}.}
  \item{data}{Data to be passed to the registered function.}
  \item{info}{Function that retrieves information about the backend.}
}
\seealso{
  \code{\link{\%dopar\%}}
}

\keyword{utilities}
