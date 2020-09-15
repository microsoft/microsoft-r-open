\name{rpart.object}
\alias{rpart.object}
\title{
  Recursive Partitioning and Regression Trees Object 
}
\description{
  These are objects representing fitted \code{rpart} trees. 
}
\section{Structure}{
  The following components must be included in a legitimate \code{rpart}
  object.
}
\value{

\item{frame}{
  data frame with one row for each node in the tree.
  The \code{row.names} of \code{frame} contain the (unique) node numbers that
  follow a binary ordering indexed by node depth.
  Columns of \code{frame} include
  \code{var}, a factor giving the names of the variables used in the
  split at each node (leaf nodes are denoted by the level \code{"<leaf>"}), 
  \code{n}, the number of observations reaching the node,
  \code{wt}, the sum of case weights for observations reaching the node,
  \code{dev}, the deviance of the node,
  \code{yval}, the fitted value of the response at the node,
  and \code{splits}, a two column matrix of left and right split labels
  for each node.  Also included in the frame are \code{complexity}, the
  complexity parameter at which this split will collapse, \code{ncompete},
  the number of competitor splits recorded, and \code{nsurrogate}, the
  number of surrogate splits recorded.

  Extra response information which may be present is in \code{yval2},
  which contains the number of events at the node (poisson tree), or a
  matrix containing the fitted class, the class counts for each node,
  the class probabilities and the \sQuote{node probability} (classification trees).
}
\item{where}{
  an integer vector of the same length as the number of observations in the
  root node, containing the row number of \code{frame} corresponding to
  the leaf node that each observation falls into. 
}
\item{call}{
  an image of the call that produced the object, but with the arguments 
  all named and with the actual formula included as the formula argument. 
  To re-evaluate the call, say \code{update(tree)}.
}
\item{terms}{
  an object of class \code{c("terms", "formula")} (see
  \code{\link{terms.object}}) summarizing the formula.  Used by various
  methods, but typically not of direct relevance to users.
}
\item{splits}{
  a numeric matrix describing the splits: only present if there are any.
  The row label is the name of
  the split variable, and columns are \code{count}, the number of
  observations (which are not missing and are of positive weight) sent
  left or right by the split (for competitor splits this is the number
  that would have been sent left or right had this split been used, for
  surrogate splits it is the number missing the primary split variable
  which were decided using this surrogate), \code{ncat}, the number of
  categories or levels for the variable (\code{+/-1} for a continuous
  variable), \code{improve}, which is the improvement in deviance given
  by this split, or, for surrogates, the concordance of the surrogate
  with the primary, and \code{index}, the numeric split point.  The last
  column \code{adj} gives the adjusted concordance for surrogate splits.
  For a factor, the \code{index} column contains the row number of the
  csplit matrix.  For a continuous variable, the sign of \code{ncat}
  determines whether the subset \code{x < cutpoint} or \code{x >
  cutpoint} is sent to the left.
}
\item{csplit}{
  an integer matrix.  (Only present only if at least one of the split
  variables is a factor or ordered factor.)  There is a row for
  each such split, and the number of columns is the largest number of
  levels in the factors.  Which row is given by the \code{index} column
  of the \code{splits} matrix.  The columns record \code{1} if that
  level of the factor goes to the left, \code{3} if it goes to the
  right, and \code{2} if that level is not present at this node
  of the tree (or not defined for the factor).
}
\item{method}{
  character string: the method used to grow the tree.   One of
  \code{"class"}, \code{"exp"}, \code{"poisson"}, \code{"anova"} or
  \code{"user"} (if splitting functions were supplied).
}
\item{cptable}{
  a matrix of information on the optimal prunings based on a
  complexity parameter.
}
\item{variable.importance}{
  a named numeric vector giving the importance of each variable.  (Only
  present if there are any splits.)  When printed by
  \code{\link{summary.rpart}} these are rescaled to add to 100.
}
\item{numresp}{
  integer number of responses; the number of levels for a factor response.
}
\item{parms, control}{
  a record of the arguments supplied, which defaults filled in.
}
\item{functions}{
  the \code{summary}, \code{print} and \code{text} functions for method used.
}
\item{ordered}{
  a named logical vector recording for each variable if it was an
  ordered factor.
}
\item{na.action}{
  (where relevant) information returned by \code{\link{model.frame}} on
  the special handling of \code{NA}s derived from the \code{na.action}
  argument.
}

There may be \link{attributes} \code{"xlevels"} and \code{"levels"}
recording the levels of any factor splitting variables and of a factor
response respectively.

Optional components include the model frame (\code{model}), the matrix
of predictors (\code{x}) and the response variable (\code{y}) used to
construct the \code{rpart} object.
}
\seealso{
  \code{\link{rpart}}.
}
\keyword{tree}
\keyword{methods}
