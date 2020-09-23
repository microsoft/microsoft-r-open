\name{idiv}
\alias{idiv}
\title{Dividing Iterator}
\description{
  Returns an iterator that returns pieces of numeric value.
}
\usage{
idiv(n, ..., chunks, chunkSize)
}
\arguments{
  \item{n}{number of times that the iterator will fire.
           If not specified, it will count forever.}
  \item{\dots}{unused.}
  \item{chunks}{the number of pieces that \code{n} should be divided into.
           This is useful when you know the number of pieces that you want.
           If specified, then \code{chunkSize} should not be.}
  \item{chunkSize}{the maximum size of the pieces that \code{n}
           should be divided into.
           This is useful when you know the size of the pieces that you want.
           If specified, then \code{chunks} should not be.}
}
\value{
  The dividing iterator.
}
\examples{
  # divide the value 10 into 3 pieces
  it <- idiv(10, chunks=3)
  nextElem(it)
  nextElem(it)
  nextElem(it)
  try(nextElem(it))  # expect a StopIteration exception

  # divide the value 10 into pieces no larger than 3
  it <- idiv(10, chunkSize=3)
  nextElem(it)
  nextElem(it)
  nextElem(it)
  nextElem(it)
  try(nextElem(it))  # expect a StopIteration exception
}
\keyword{utilities}
