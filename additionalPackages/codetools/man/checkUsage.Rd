\name{checkUsage}
\title{Check R Code for Possible Problems}
\usage{
checkUsage(fun, name = "<anonymous>", report = cat, all = FALSE, 
           suppressLocal = FALSE, suppressParamAssigns = !all,
           suppressParamUnused = !all, suppressFundefMismatch = FALSE,
           suppressLocalUnused = FALSE, suppressNoLocalFun = !all,
           skipWith = FALSE, suppressUndefined = dfltSuppressUndefined,
           suppressPartialMatchArgs = TRUE) 
checkUsageEnv(env, ...) 
checkUsagePackage(pack, ...) 
}
\alias{checkUsage}
\alias{checkUsageEnv}
\alias{checkUsagePackage}
\arguments{
  \item{fun}{closure.}
  \item{name}{character; name of closure.}
  \item{env}{environment containing closures to check.}
  \item{pack}{character naming package to check.}
  \item{\dots}{options to be passed to \code{checkUsage}.}
  \item{report}{function to use to report possible problems.}
  \item{all}{logical; report all possible problems if TRUE.}
  \item{suppressLocal}{suppress all local variable warnings.}
  \item{suppressParamAssigns}{suppress warnings about assignments to formal
                              parameters.}
  \item{suppressParamUnused}{suppress warnings about unused formal
                             parameters.}
  \item{suppressFundefMismatch}{suppress warnings about multiple local
                                function definitions with different formal
                                argument lists}
  \item{suppressLocalUnused}{suppress warnings about unused local variables}
  \item{suppressNoLocalFun}{suppress warnings about using local variables
                            as functions with no apparent local function
                            definition}
  \item{skipWith}{logical; if true, do no examine code portion of
                  \code{with} expressions.}
  \item{suppressUndefined}{suppress warnings about undefined global
                          functions and variables.}

  \item{suppressPartialMatchArgs}{suppress warnings about partial
                                  argument matching}
}
\description{
  Check R code for possible problems.
}
\details{
  \code{checkUsage} checks a single R closure.  Options control which
  possible problems to report.  The default settings are moderately
  verbose.  A first pass might use \code{suppressLocal=TRUE} to
  suppress all information related to local variable usage.
  The \code{suppressXYZ} values can either be scalar logicals or
  character vectors; then they are character vectors they only
  suppress problem reports for the variables with names in the vector.

  \code{checkUsageEnv} and \code{checkUsagePackage} are convenience
  functions that apply \code{checkUsage} to all closures in an
  environment or a package.  \code{checkUsagePackage} requires that the
  package be loaded.  If the package has a name space then the internal
  name space frame is checked.
}
\author{Luke Tierney}

\examples{
checkUsage(checkUsage)
checkUsagePackage("codetools",all=TRUE)
\dontrun{checkUsagePackage("base",suppressLocal=TRUE)}
}
\keyword{programming}
