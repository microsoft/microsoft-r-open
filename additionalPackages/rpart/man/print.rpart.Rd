\name{print.rpart}
\alias{print.rpart}
\title{
  Print an Rpart Object 
}
\description{
This function prints an \code{rpart} object.  It is a method for the generic 
function \code{print} of class \code{"rpart"}. 
}
\usage{
\method{print}{rpart}(x, minlength = 0, spaces = 2, cp, digits = getOption("digits"), \dots)
}
\arguments{
\item{x}{
fitted model object of class \code{"rpart"}.  This is assumed to be the result 
of some function that produces an object with the same named 
components as that returned by the \code{rpart} function. 
}
\item{minlength}{
Controls the abbreviation of labels: see \code{\link{labels.rpart}}.
}
\item{spaces}{
the number of spaces to indent nodes of increasing depth. 
}
\item{digits}{
the number of digits of numbers to print. 
}
\item{cp}{
  prune all nodes with a complexity less than \code{cp} from the
  printout. Ignored if unspecified.
}
\item{\dots}{
  arguments to be passed to or from other methods.
}}
\section{Side Effects}{
  A semi-graphical layout of the contents of \code{x$frame} is
  printed.  Indentation is used to convey the tree topology.
  Information for each node includes the node number, split, size,
  deviance, and fitted value.  For the \code{"class"} method, the
  class probabilities are also printed. 
}
\details{
  This function is a method for the generic function \code{print} for class 
  \code{"rpart"}.  It can be invoked by calling print for an object of the 
  appropriate class, or directly by calling \code{print.rpart} regardless of 
  the class of the object. 
}
\seealso{
  \code{\link{print}},  \code{\link{rpart.object}},
  \code{\link{summary.rpart}},  \code{\link{printcp}}   
}
\examples{
z.auto <- rpart(Mileage ~ Weight, car.test.frame)
z.auto
\dontrun{node), split, n, deviance, yval
      * denotes terminal node

 1) root 60 1354.58300 24.58333  
   2) Weight>=2567.5 45  361.20000 22.46667  
     4) Weight>=3087.5 22   61.31818 20.40909 *
     5) Weight<3087.5 23  117.65220 24.43478  
      10) Weight>=2747.5 15   60.40000 23.80000 *
      11) Weight<2747.5 8   39.87500 25.62500 *
   3) Weight<2567.5 15  186.93330 30.93333 *
}}
\keyword{tree}
