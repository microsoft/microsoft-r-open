\name{prune.rpart}
\alias{prune.rpart}
\alias{prune}
\title{
  Cost-complexity Pruning of an Rpart Object 
}
\description{
Determines a nested sequence of subtrees of the supplied \code{rpart} object 
by recursively \code{snipping} off the least important splits, based on the 
complexity parameter (\code{cp}). 
}
\usage{
prune(tree, \dots)

\method{prune}{rpart}(tree, cp, \dots)
}
\arguments{
\item{tree}{
fitted model object of class \code{"rpart"}.  This is assumed to be the result 
of some function that produces an object with the same named 
components as that returned by the \code{rpart} function. 
}
\item{cp}{
  Complexity parameter to which the \code{rpart} object will be trimmed.
}
\item{\dots}{further arguments passed to or from other methods.}
}
\value{
A new \code{rpart} object that is trimmed to the value \code{cp}. 
}
\seealso{
\code{\link{rpart}} 
}
\examples{
z.auto <- rpart(Mileage ~ Weight, car.test.frame)
zp <- prune(z.auto, cp = 0.1)
plot(zp) #plot smaller rpart object
}
\keyword{tree}
