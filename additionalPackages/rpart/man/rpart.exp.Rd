\name{rpart.exp}
\alias{rpart.exp}
\title{Initialization function for exponential fitting}
\description{
  This function does the initialization step for rpart, when the
  response is a survival object.  It rescales the data so as to have an
  exponential baseline hazard and then uses Poisson methods.  This
  function would rarely if ever be called directly by a user.
}
\usage{
rpart.exp(y, offset, parms, wt)}
\arguments{
  \item{y}{the response, which will be of class \code{Surv}}
  \item{offset}{optional offset}
  \item{parms}{parameters controlling the fit.
    This is a list with components \code{shrink} and \code{method}.
    The first is the prior for the coefficient of variation of the
    predictions.  The second is either \code{"deviance"}
    or \code{"sqrt"} and is the measure used for cross-validation.
    If values are missing the defaults are used, which are
    \code{"deviance"} for the method, and a shrinkage of 1.0 for the
    deviance method and 0 for the square root.}
  \item{wt}{case weights, if present}
}
\value{a list with the necessary initialization components}
\author{Terry Therneau}
\seealso{\code{\link{rpart}}}
\keyword{ tree }
