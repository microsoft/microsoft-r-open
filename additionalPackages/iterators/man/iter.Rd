\name{iter}
\alias{iter}
\alias{iter.default}
\alias{iter.iter}
\alias{iter.matrix}
\alias{iter.data.frame}
\alias{iter.function}
\title{Iterator Factory Functions}
\description{
  \code{iter} is a generic function used to create iterator objects.
}
\usage{
iter(obj, \dots)

\method{iter}{default}(obj, checkFunc=function(...) TRUE, recycle=FALSE,
\dots)
\method{iter}{iter}(obj, \dots)
\method{iter}{matrix}(obj, by=c('column', 'cell', 'row'), chunksize=1L,
checkFunc=function(...) TRUE, recycle=FALSE, \dots)
\method{iter}{data.frame}(obj, by=c('column', 'row'),
checkFunc=function(...) TRUE, recycle=FALSE, \dots)
\method{iter}{function}(obj, checkFunc=function(...) TRUE,
recycle=FALSE, \dots)
}
\arguments{
  \item{obj}{an object from which to generate an iterator.}
  \item{by}{how to iterate.}
  \item{chunksize}{the number of elements of \code{by} to return with
    each call to \code{nextElem}.}
  \item{checkFunc}{a function which, when passed an iterator value,
    return \code{TRUE} or \code{FALSE}.  If \code{FALSE}, the value is
    skipped in the iteration.}
  \item{recycle}{a boolean describing whether the iterator should reset
    after running through all it's values.}
  \item{\dots}{additional arguments affecting the iterator.}
}
\value{
  The iterator.
}
\examples{
  # a vector iterator
  i1 <- iter(1:3)
  nextElem(i1)
  nextElem(i1)
  nextElem(i1)

  # a vector iterator with a checkFunc
  i1 <- iter(1:3, checkFunc=function(i) i \%\% 2 == 0)
  nextElem(i1)

  # a data frame iterator by column
  i2 <- iter(data.frame(x=1:3, y=10, z=c('a', 'b', 'c')))
  nextElem(i2)
  nextElem(i2)
  nextElem(i2)

  # a data frame iterator by row
  i3 <- iter(data.frame(x=1:3, y=10), by='row')
  nextElem(i3)
  nextElem(i3)
  nextElem(i3)

  # a function iterator
  i4 <- iter(function() rnorm(1))
  nextElem(i4)
  nextElem(i4)
  nextElem(i4)
}
\keyword{methods}
