\name{icount}
\alias{icount}
\alias{icountn}
\title{Counting Iterators}
\description{
  Returns an iterator that counts starting from one.
}
\usage{
icount(count)
icountn(vn)
}
\arguments{
  \item{count}{number of times that the iterator will fire.
  If not specified, it will count forever.}
  \item{vn}{vector of counts.}
}
\value{
  The counting iterator.
}
\examples{
  # create an iterator that counts from 1 to 3.
  it <- icount(3)
  nextElem(it)
  nextElem(it)
  nextElem(it)
  try(nextElem(it))  # expect a StopIteration exception
}
\keyword{utilities}
