\name{nextElem}
\alias{nextElem}
\alias{nextElem.containeriter}
\alias{nextElem.funiter}
\title{Get Next Element of Iterator}
\description{
  \code{nextElem} is a generic function used to produce values. If a
  \code{checkFunc} was specified to the constructor, the potential
  iterated values will be passed to the \code{checkFunc} until the
  \code{checkFunc} returns \code{TRUE}. When the iterator has no more
  values, it calls stop with the message 'StopIteration'.
}
\usage{
nextElem(obj, \dots)

\method{nextElem}{containeriter}(obj, \dots)
\method{nextElem}{funiter}(obj, \dots)
}
\arguments{
  \item{obj}{an iterator object.}
  \item{\dots}{additional arguments that are ignored.}
}
\value{
  The value.
}
\examples{
  it <- iter(c('a', 'b', 'c'))
  print(nextElem(it))
  print(nextElem(it))
  print(nextElem(it))
}
\keyword{methods}
