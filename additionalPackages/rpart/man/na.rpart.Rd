\name{na.rpart}
\alias{na.rpart}
\title{
  Handles Missing Values in an Rpart Object 
}
\usage{
na.rpart(x)
}
\description{
  Handles missing values in an \code{"rpart"} object.
}
\arguments{
\item{x}{
  a model frame.
}}
\details{
  Default function that handles missing values when calling the
  function \code{rpart}.

  It omits cases where part of the response is missing or all the
  explanatory variables are missing.
}
\keyword{tree}
