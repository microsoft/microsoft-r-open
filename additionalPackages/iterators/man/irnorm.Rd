\name{irnorm}
\alias{irnorm}
\alias{irunif}
\alias{irbinom}
\alias{irnbinom}
\alias{irpois}
\title{Random Number Iterators}
\description{
  These function returns an iterators that return random numbers
  of various distributions.
  Each one is a wrapper around a standard \code{R} function.
}
\usage{
irnorm(..., count)
irunif(..., count)
irbinom(..., count)
irnbinom(..., count)
irpois(..., count)
}
\arguments{
  \item{count}{number of times that the iterator will fire.
               If not specified, it will fire values forever.}
  \item{\dots}{arguments to pass to the underlying \code{rnorm} function.}
}
\value{
  An iterator that is a wrapper around the corresponding random number
  generator function.
}
\examples{
  # create an iterator that returns three random numbers
  it <- irnorm(1, count=3)
  nextElem(it)
  nextElem(it)
  nextElem(it)
  try(nextElem(it))  # expect a StopIteration exception
}
\keyword{utilities}
