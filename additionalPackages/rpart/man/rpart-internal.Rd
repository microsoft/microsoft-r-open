\name{rpart-internal}
\alias{pred.rpart}
\alias{rpart.matrix}
\title{
  Internal Functions 
}
\description{
  Internal functions, only used by packages \pkg{rpart} and \pkg{ipred}.
}
\usage{
pred.rpart(fit, x)
rpart.anova(y, offset, parms, wt)
rpart.class(y, offset, parms, wt)
rpart.matrix(frame)
rpart.poisson(y, offset, parms, wt)
rpartco(tree, parms)
}
\arguments{
  \item{fit}{a tree fitted by \code{rpart}.}
  \item{x}{a matrix of predictors.}
  \item{y}{the responses.}
  \item{offset}{an offset, or \code{NULL}.}
  \item{parms}{a list of parameters, usually empty.}
  \item{wt}{case weights.}
  \item{frame}{model frame (from call to \code{rpart})}
  \item{tree}{a tree fitted by \code{rpart}.}
}
\value{
  For \code{rpartco} the x,y plotting coordinates of the nodes.
  
  \code{rpart.anova}, \code{rpart.class} and
  \code{rpart.poisson} return a list with components
  \item{y}{(adjusting for \code{offset} if necessary),}
  \item{parms}{as input,}
  \item{numresp}{the number of responses,}
  \item{summary}{a function to be invoked by \code{\link{summary.rpart}},}
  \item{text}{a function to be invoked by \code{\link{text.rpart}}.}
}
\keyword{internal}
