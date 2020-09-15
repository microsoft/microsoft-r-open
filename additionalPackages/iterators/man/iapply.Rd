\name{iapply}
\alias{iapply}
\title{Array/Apply Iterator}
\description{
  Returns an iterator over an array, which iterates over the 
  array in much the same manner
  as the \code{apply} function.
}
\usage{
iapply(X, MARGIN)
}
\arguments{
  \item{X}{the array to iterate over.}
  \item{MARGIN}{a vector of subscripts.
    \code{1} indicates the first dimension (rows), \code{2}
    indicates the second dimension (columns), etc.}
}
\value{
  The apply iterator.
}
\seealso{
  \code{\link{apply}}
}
\examples{
a <- array(1:8, c(2, 2, 2))

# iterate over all the matrices
it <- iapply(a, 3)
as.list(it)

# iterate over all the columns of all the matrices
it <- iapply(a, c(2, 3))
as.list(it)

# iterate over all the rows of all the matrices
it <- iapply(a, c(1, 3))
as.list(it)
}
\keyword{utilities}
