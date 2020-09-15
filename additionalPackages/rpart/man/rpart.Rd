\name{rpart}
\alias{rpart}
%\alias{rpartcallback}
\title{
  Recursive Partitioning and Regression Trees
}
\description{
  Fit a \code{rpart} model
}
\usage{
rpart(formula, data, weights, subset, na.action = na.rpart, method,
      model = FALSE, x = FALSE, y = TRUE, parms, control, cost, \dots)
}
\arguments{
  \item{formula}{a \link{formula}, with a response but no interaction
    terms.  If this a a data frame, that is taken as the model frame
    (see \code{\link{model.frame}).}
  }
  
  \item{data}{an optional data frame in which to interpret the variables
    named in the formula.} 
  
  \item{weights}{optional case weights.}
  
  \item{subset}{optional expression saying that only a subset of the
    rows of the data should be used in the fit.}
  
  \item{na.action}{the default action deletes all observations for which
    \code{y} is missing, but keeps those in which one or more predictors
    are missing.}
  
  \item{method}{one of \code{"anova"}, \code{"poisson"}, \code{"class"}
    or \code{"exp"}.  If \code{method} is missing then the routine tries
    to make an intelligent guess.
    If \code{y} is a survival object, then \code{method = "exp"} is assumed,
    if \code{y} has 2 columns then \code{method = "poisson"} is assumed,
    if \code{y} is a factor then \code{method = "class"} is assumed,
    otherwise \code{method = "anova"} is assumed.
    It is wisest to specify the method directly, especially as more
    criteria may added to the function in future.

    Alternatively, \code{method} can be a list of functions named
    \code{init}, \code{split} and \code{eval}.  Examples are given in
    the file \file{tests/usersplits.R} in the sources, and in the
    vignettes \sQuote{User Written Split Functions}.}
  
  \item{model}{if logical: keep a copy of the model frame in the result?
    If the input value for \code{model} is a model frame (likely from an
    earlier call to the \code{rpart} function), then this frame is used
    rather than constructing new data.}
  \item{x}{keep a copy of the \code{x} matrix in the result.}
  
  \item{y}{keep a copy of the dependent variable in the result.  If
    missing and \code{model} is supplied this defaults to \code{FALSE}.}
  
  \item{parms}{optional parameters for the splitting function.\cr
    Anova splitting has no parameters.\cr
    Poisson splitting has a single parameter, the coefficient of variation of
    the prior distribution on the rates.  The default value is 1.\cr
    Exponential splitting has the same parameter as Poisson.\cr
    For classification splitting, the list can contain any of:
    the vector of prior probabilities (component \code{prior}), the loss matrix
    (component \code{loss}) or the splitting index (component
    \code{split}).  The priors must be positive and sum to 1.  The loss
    matrix must have zeros on the diagonal and positive off-diagonal
    elements.  The splitting index can be \code{gini} or
    \code{information}.  The default priors are proportional to the data
    counts, the losses default to 1, and the split defaults to
    \code{gini}.}
  
  \item{control}{a list of options that control details of the
    \code{rpart} algorithm.  See \code{\link{rpart.control}}.}
  
  \item{cost}{a vector of non-negative costs, one for each variable in
    the model. Defaults to one for all variables.  These are scalings to
    be applied when considering splits, so the improvement on splitting
    on a variable is divided by its cost in deciding which split to
    choose.}
  
  \item{\dots}{arguments to \code{\link{rpart.control}} may also be
    specified in the call to \code{rpart}.  They are checked against the
    list of valid arguments.}
}

\details{
  This differs from the \code{tree} function in S mainly in its handling
  of surrogate variables.  In most details it follows Breiman
  \emph{et. al} (1984) quite closely.  \R package \pkg{tree} provides a
  re-implementation of \code{tree}.
}

\value{
  An object of class \code{rpart}.  See \code{\link{rpart.object}}.
}

\references{
  Breiman L., Friedman J. H., Olshen R. A., and Stone, C. J. (1984)
  \emph{Classification and Regression Trees.}
  Wadsworth.
}

\seealso{
  \code{\link{rpart.control}}, \code{\link{rpart.object}},
  \code{\link{summary.rpart}}, \code{\link{print.rpart}}
}

\examples{
fit <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis)
fit2 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              parms = list(prior = c(.65,.35), split = "information"))
fit3 <- rpart(Kyphosis ~ Age + Number + Start, data = kyphosis,
              control = rpart.control(cp = 0.05))
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit)
text(fit, use.n = TRUE)
plot(fit2)
text(fit2, use.n = TRUE)
}
\keyword{tree}
