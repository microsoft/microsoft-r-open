\name{path.rpart}
\alias{path.rpart}
\title{
  Follow Paths to Selected Nodes of an Rpart Object
}
\description{
  Returns a names list where each element contains the splits on the
  path from the root to the selected nodes.
}
\usage{
path.rpart(tree, nodes, pretty = 0, print.it = TRUE)
}
\arguments{
\item{tree}{
  fitted model object of class \code{"rpart"}.  This is assumed to be the
  result of some function that produces an object with the same named
  components as that returned by the \code{rpart} function.
}
\item{nodes}{
  an integer vector containing indices (node numbers) of all nodes for
  which paths are desired.  If missing, user selects nodes as described
  below. 
}
\item{pretty}{
  an integer denoting the extent to which factor levels in split labels
  will be abbreviated.  A value of (0) signifies no abbreviation.  A
  \code{NULL}, the default, signifies using elements of letters to represent
  the different factor levels.
}
\item{print.it}{
  Logical. Denotes whether paths will be printed out as
  nodes are interactively selected.   Irrelevant if \code{nodes}
  argument is supplied.
}}
\value{
  A named (by node) list, each element of which contains all
  the  splits  on the path from the root to the specified or
  selected nodes.
}
\section{Graphical Interaction}{
  A dendrogram of the \code{rpart} object is expected to be visible on
  the graphics device, and a graphics input device (e.g. a mouse) is
  required.  Clicking (the selection button) on a node selects that
  node.  This process may be repeated any number of times.  Clicking the
  exit button will stop the selection process and return the list of
  paths.
}
\details{
  The function has a required argument as an \code{rpart} object  and
  a list of nodes as optional arguments.  Omitting a list of
  nodes will cause the function to  wait  for  the  user  to
  select  nodes  from the dendrogram.  It will return a list,
  with one component for each node  specified  or  selected.
  The  component  contains the sequence of splits leading to
  that node.  In the graphical  interaction,  the  individual
  paths are printed out as nodes are selected.
}
\references{
  This function was modified from \code{path.tree} in S. 
}
\seealso{
  \code{\link{rpart}}
}
\examples{
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
print(fit)
path.rpart(fit, node = c(11, 22))
}
\keyword{tree}
