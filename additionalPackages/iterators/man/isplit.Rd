\name{isplit}
\alias{isplit}
\title{Split Iterator}
\description{
  Returns an iterator that divides the data in the vector \code{x}
  into the groups defined by \code{f}.
}
\usage{
isplit(x, f, drop=FALSE, \dots)
}
\arguments{
  \item{x}{vector or data frame of values to be split into groups.}
  \item{f}{a factor or list of factors used to categorize \code{x}.}
  \item{drop}{logical indicating if levels that do not occur should be dropped.}
  \item{\dots}{current ignored.}
}
\value{
  The split iterator.
}
\seealso{
  \code{\link{split}}
}
\examples{
x <- rnorm(200)
f <- factor(sample(1:10, length(x), replace=TRUE))

it <- isplit(x, f)
expected <- split(x, f)

for (i in expected) {
  actual <- nextElem(it)
  stopifnot(actual$value == i)
}
}
\keyword{utilities}
