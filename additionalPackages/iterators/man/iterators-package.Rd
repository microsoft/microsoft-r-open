\name{iterators-package}
\alias{iterators-package}
\alias{iterators}
\docType{package}
\title{
The Iterators Package
}
\description{
The iterators package provides tools for iterating over various R data structures.
Iterators are available for vectors, lists, matrices, data frames, and files.
By following very simple conventions, new iterators can be written to support
any type of data source, such as database queries or dynamically generated
data.
}
\details{
Further information is available in the following help topics:
\tabular{ll}{
\code{iter} \tab Generic function used to create iterator objects.\cr
\code{nextElem} \tab Generic function used to get the next element of a iterator.\cr
\code{icount} \tab A function used to create a counting iterator.\cr
\code{idiv} \tab A function used to create a number dividing iterator.\cr
\code{ireadLines} \tab A function used to create a file reading iterator.\cr
}

For a complete list of functions with individual help pages,
use \code{library(help="iterators")}.
}
\keyword{package}
