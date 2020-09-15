\name{doParallel-package}
\alias{doParallel-package}
\alias{doParallel}
\docType{package}
\title{
The doParallel Package
}
\description{
The doParallel package provides a parallel backend for the foreach/\%dopar\%
function using the \code{parallel} package of R 2.14.0 and later.
}
\details{
Further information is available in the following help topics:
\tabular{ll}{
\code{registerDoParallel} \tab register doParallel to be used by foreach/\%dopar\%\cr
}

To see a tutorial introduction to the doParallel package,
use \code{vignette("gettingstartedParallel")}.  To see a tutorial
introduction to the foreach package, use \code{vignette("foreach")}.

To see a demo of doParallel computing the sinc function,
use \code{demo(sincParallel)}.

Some examples (in addition to those in the help pages) are included in
the ``examples'' directory of the doParallel package.  To list the files in
the examples directory,
use \code{list.files(system.file("examples", package="doParallel"))}.
To run the bootstrap example, use
\code{source(system.file("examples", "bootParallel.R", package="doParallel"))}.
This is a simple benchmark, executing both sequentally and in parallel.
There are many more examples that come with the foreach package,
which will work with the doParallel package if it is registered as the
parallel backend.

For a complete list of functions with individual help pages,
use \code{library(help="doParallel")}.
}
\keyword{package}
