\name{foreach-ext}
\alias{foreach-ext}
\alias{makeAccum}
\alias{accumulate}
\alias{getexports}
\alias{getResult}
\alias{getErrorValue}
\alias{getErrorIndex}
\title{Foreach Extension Functions}
\description{
These functions are used to write parallel backends for the \code{foreach}
package.  They should not be used from normal scripts or packages that use
the \code{foreach} package.
}
\usage{
makeAccum(it)
accumulate(obj, result, tag, ...)
getexports(ex, e, env, good=character(0), bad=character(0))
getResult(obj, \dots)
getErrorValue(obj, \dots)
getErrorIndex(obj, \dots)
}
\arguments{
  \item{it}{foreach iterator.}
  \item{ex}{call object to analyze.}
  \item{e}{local environment of the call object.}
  \item{env}{exported environment in which call object will be evaluated.}
  \item{good}{names of symbols that are being exported.}
  \item{bad}{names of symbols that are not being exported.}
  \item{obj}{foreach iterator object.}
  \item{result}{task result to accumulate.}
  \item{tag}{tag of task result to accumulate.}
  \item{\dots}{unused.}
}
\note{
These functions are likely to change in future versions of the
\code{foreach} package.  When they become more stable, they will
be documented.
}

\keyword{utilities}
