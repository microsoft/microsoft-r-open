\name{setDoPar}
\alias{setDoPar}
\title{setDoPar}
\description{
The \code{setDoPar} function is used to register a parallel backend with the
foreach package.  This isn't normally executed by the user.  Instead, packages
that provide a parallel backend provide a function named \code{registerDoPar}
that calls \code{setDoPar} using the appropriate arguments.
}
\usage{
setDoPar(fun, data=NULL, info=function(data, item) NULL)
}
\arguments{
  \item{fun}{A function that implements the functionality of \code{\%dopar\%}.}
  \item{data}{Data to passed to the registered function.}
  \item{info}{Function that retrieves information about the backend.}
}
\seealso{
  \code{\link{\%dopar\%}}
}

\keyword{utilities}
