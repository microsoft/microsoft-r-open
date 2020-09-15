\name{foreach-package}
\alias{foreach-package}
\docType{package}
\title{
The Foreach Package
}
\description{
The foreach package provides a looping construct for executing
R code repeatedly.  The main reason for using the foreach package
is that it supports parallel execution.  The foreach package can
be used with a variety of different parallel computing systems,
including MPI, Azure Batch, NetWorkSpaces and snow.  In addition, 
foreach can be
used with iterators, which allows the data to specified in a very
flexible way.
}
\details{
Further information is available in the following help topics:
\tabular{ll}{
\code{foreach} \tab Specify the variables to iterate over\cr
\code{\%do\%} \tab Execute the R expression sequentially\cr
\code{\%dopar\%} \tab Execute the R expression using the currently registered backend\cr
}

To see a tutorial introduction to the foreach package,
use \code{vignette("foreach")}.

To see a demo of foreach computing the sinc function,
use \code{demo(sincSEQ)}.

Some examples (in addition to those in the help pages) are included in
the ``examples'' directory of the foreach package.  To list the files in
the examples directory,
use \code{list.files(system.file("examples", package="foreach"))}.
To run the bootstrap example, use
\code{source(system.file("examples", "bootseq.R", package="foreach"))}.

For a complete list of functions with individual help pages,
use \code{library(help="foreach")}.
}
\keyword{package}
