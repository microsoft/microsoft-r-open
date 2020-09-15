\name{xpred.rpart}
\alias{xpred.rpart}
\title{
  Return Cross-Validated Predictions 
}
\description{
  Gives the predicted values for an \code{rpart} fit, under
  cross validation, for a set of complexity parameter values.
}
\usage{
xpred.rpart(fit, xval = 10, cp, return.all = FALSE)
}
\arguments{
\item{fit}{
  a object of class \code{"rpart"}. 
}
\item{xval}{
  number of cross-validation groups.  This may also be an explicit list of
  integers that define the cross-validation groups.
}
\item{cp}{
  the desired list of complexity values.  By default it is taken from the
  \code{cptable} component of the fit.
}
\item{return.all}{
  if FALSE return only the first element of the prediction}
}
\value{
  A matrix with one row for each observation and one column for each complexity
  value.  If \code{return.all} is TRUE and the prediction for each node
  is a vector, then the result will be an array containing all of the
  predictions.  When the response is categorical, for instance, the
  result contains the predicted class followed by the class
  probabilities of the selected terminal node;
  \code{result[1,,]} will be the matrix of predicted classes,
  \code{result[2,,]} the matrix of class 1 probabilities, etc.
}
\details{
  Complexity penalties are actually ranges, not values.  If the
  \code{cp} values found in the table were \eqn{.36}, \eqn{.28},
  and \eqn{.13}, for instance, this means that the first row of the
  table holds for all complexity penalties in the range \eqn{[.36, 1]},
  the second row for \code{cp} in the range \eqn{[.28, .36)} and
  the third row for \eqn{[.13,.28)}.  By default, the geometric mean
  of each interval is used for cross validation.
}
\seealso{
\code{\link{rpart}} 
}
\examples{
fit <- rpart(Mileage ~ Weight, car.test.frame)
xmat <- xpred.rpart(fit)
xerr <- (xmat - car.test.frame$Mileage)^2
apply(xerr, 2, sum)   # cross-validated error estimate

# approx same result as rel. error from printcp(fit)
apply(xerr, 2, sum)/var(car.test.frame$Mileage) 
printcp(fit)
}
\keyword{tree}
