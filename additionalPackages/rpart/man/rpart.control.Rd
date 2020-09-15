\name{rpart.control}
\alias{rpart.control}
\title{
  Control for Rpart Fits
}
\description{
Various parameters that control aspects of the \code{rpart} fit. 
}
\usage{
rpart.control(minsplit = 20, minbucket = round(minsplit/3), cp = 0.01, 
              maxcompete = 4, maxsurrogate = 5, usesurrogate = 2, xval = 10,
              surrogatestyle = 0, maxdepth = 30, \dots)
}
\arguments{
  \item{minsplit}{
    the minimum number of observations that must exist in a node in order for 
    a split to be attempted. 
  }
  \item{minbucket}{
    the minimum number of observations in any terminal \code{<leaf>} node.
    If only one of \code{minbucket} or \code{minsplit} is specified,
    the code either sets \code{minsplit} to \code{minbucket*3}
    or \code{minbucket} to \code{minsplit/3}, as appropriate.
  }
  \item{cp}{
    complexity parameter.  Any split that does not decrease the overall
    lack of fit by a factor of \code{cp} is not attempted.  For instance,
    with \code{anova} splitting, this means that the overall R-squared must
    increase by \code{cp} at each step. The main role of this parameter
    is to save computing time by pruning off splits that are obviously
    not worthwhile. Essentially,the user informs the program that any
    split which does not improve the fit by \code{cp} will likely be
    pruned off by cross-validation, and that hence the program need
    not pursue it.
  }
  \item{maxcompete}{
    the number of competitor splits retained in the output.  It is useful to
    know not just which split was chosen, but which variable came in second,
    third, etc.
  }
  \item{maxsurrogate}{
    the number of surrogate splits retained in the output.  If this is set to
    zero the compute time will be reduced, since approximately half of the
    computational time (other than setup) is used in the search for surrogate
    splits.
  }
  \item{usesurrogate}{
    how to use surrogates in the splitting process.  \code{0} means
    display only; an observation with a missing value for the primary
    split rule is not sent further down the tree.  \code{1} means use
    surrogates, in order, to split subjects missing the primary variable;
    if all surrogates are missing the observation is not split.  For value
    \code{2} ,if all surrogates are missing, then send the observation in
    the majority direction.  A value of \code{0} corresponds to the action
    of \code{tree}, and \code{2} to the recommendations of Breiman
    \emph{et.al} (1984).
  }
  \item{xval}{
    number of cross-validations.
  }
  \item{surrogatestyle}{
    controls the selection of a best surrogate.
    If set to \code{0} (default) the program uses the total number of correct
    classification for a potential surrogate variable,
    if set to \code{1} it uses the percent correct, calculated over the
    non-missing values of the surrogate.
    The first option more severely penalizes covariates with a large number of
    missing values.
  }
  \item{maxdepth}{
    Set the maximum depth of any node of the final tree, with the root
    node counted as depth 0.  Values greater than  30 \code{rpart} will
    give nonsense results on 32-bit machines.
  }
  \item{\dots}{
    mop up other arguments.
  }
}
\value{
  A list containing the options.
}
\seealso{
  \code{\link{rpart}}
}
\keyword{tree}
