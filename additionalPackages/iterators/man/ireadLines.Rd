\name{ireadLines}
\alias{ireadLines}
\title{Iterator over Lines of Text from a Connection}
\description{
  Returns an iterator over the lines of text from a connection.
  It is a wrapper around the standard \code{readLines} function.
}
\usage{
ireadLines(con, n=1, ...)
}
\arguments{
  \item{con}{a connection object or a character string.}
  \item{n}{integer.  The maximum number of lines to read.
           Negative values indicate that one should read up to
           the end of the connection.  The default value is 1.}
  \item{\dots}{passed on to the \code{readLines} function.}
}
\value{
  The line reading iterator.
}
\seealso{
  \code{\link[base]{readLines}}
}
\examples{
  # create an iterator over the lines of COPYING
  it <- ireadLines(file.path(R.home(), 'COPYING'))
  nextElem(it)
  nextElem(it)
  nextElem(it)
}
\keyword{utilities}
