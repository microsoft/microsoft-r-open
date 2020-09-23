\name{plotcp}
\alias{plotcp}
\title{
  Plot a Complexity Parameter Table for an Rpart Fit 
}
\description{
Gives a visual representation of the cross-validation results in an 
\code{rpart} object. 
}
\usage{
plotcp(x, minline = TRUE, lty = 3, col = 1,
       upper = c("size", "splits", "none"), \dots)
}
\arguments{
\item{x}{
an object of class \code{"rpart"} 
}
\item{minline}{
whether a horizontal line is drawn 1SE above the minimum of the curve. 
}
\item{lty}{
line type for this line 
}
\item{col}{
colour for this line 
}
\item{upper}{
what is plotted on the top axis: the size of the tree (the number of
leaves), the number of splits or nothing. 
}
\item{\dots}{
additional plotting parameters 
}
}
\value{
None. 
}
\section{Side Effects}{
A plot is produced on the current graphical device. 
}
\details{
The set of possible cost-complexity prunings of a tree from a nested
set. For the geometric means of the intervals of values of \code{cp} for which
a pruning is optimal, a cross-validation has (usually) been done in
the initial construction by \code{\link{rpart}}. The \code{cptable} in the fit contains
the mean and standard deviation of the errors in the cross-validated
prediction against each of the geometric means, and these are plotted
by this function. A good choice of \code{cp} for pruning is often the
leftmost value for which the mean lies below the horizontal line.
}
\seealso{
\code{\link{rpart}},  \code{\link{printcp}},  \code{\link{rpart.object}}   
}
\keyword{tree}
