\name{xpred.rpart}
\alias{xpred.rpart}
\title{
  Return Cross-Validated Predictions 
}
\description{
  Gives the predicted values for an \code{rpart} fit, under
  cross validation, for a set of complexity parameter values.
  In Revolution R Enterprise, this function is generic, with a
  method for \code{rxDTree} objects (that currently stops
  with the message that the method is unimplemented); the default 
  method is the original \code{rpart} version.
}
\usage{
xpred.rpart(fit, xval=10, cp)
}
\details{
For complete details, see \code{\link[rpart]{xpred.rpart}}.
}
\keyword{tree}
