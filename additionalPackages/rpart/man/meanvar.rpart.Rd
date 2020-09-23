\name{meanvar.rpart}
\alias{meanvar}
\alias{meanvar.rpart}
\title{
  Mean-Variance Plot for an Rpart Object 
}
\description{
Creates a plot on the current graphics device of the deviance of the 
node divided by the number of observations at the node.  Also returns 
the node number. 
}
\usage{
meanvar(tree, \dots)

\method{meanvar}{rpart}(tree, xlab = "ave(y)", ylab = "ave(deviance)", \dots)
}
\arguments{
\item{tree}{
fitted model object of class \code{"rpart"}.  This is assumed to be the result 
of some function that produces an object with the same named 
components as that returned by the \code{rpart} function. 
}
\item{xlab}{
x-axis label for the plot. 
}
\item{ylab}{
y-axis label for the plot. 
}
\item{\dots}{
additional graphical parameters may be supplied as arguments to this function. 
}
}
\value{
an invisible list containing the following vectors is returned. 

\item{x}{
fitted value at terminal nodes (\code{yval}). 
}
\item{y}{
deviance of node divided by number of observations at node. 
}
\item{label}{
node number. 
}
}
\section{Side Effects}{
a plot is put on the current graphics device. 
}
\seealso{
 \code{\link{plot.rpart}}.   
}
\examples{
z.auto <- rpart(Mileage ~ Weight, car.test.frame)
meanvar(z.auto, log = 'xy')
}
\keyword{tree}
