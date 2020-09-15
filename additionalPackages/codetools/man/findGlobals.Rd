\name{findGlobals}
\title{Find Global Functions and Variables Used by a Closure}
\usage{
findGlobals(fun, merge = TRUE) 
}
\alias{findGlobals}
\arguments{
  \item{fun}{function object; usually a closure.}
  \item{merge}{logical}
}
\value{
  Character vector if \code{merge} is true; otherwise, a list with
  \code{functions} and \code{variables} character vector components.
  Character vectors are of length zero For non-closures.
}
\description{
  Finds global functions and variables used by a closure.
}
\details{
  The result is an approximation. R semantics only allow variables
  that might be local to be identified (and event that assumes no use
  of \code{assign} and \code{rm}).
}
\author{Luke Tierney}

\examples{
findGlobals(findGlobals)
findGlobals(findGlobals, merge = FALSE)
}
\keyword{programming}
