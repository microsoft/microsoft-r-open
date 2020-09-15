\name{showTree}
\title{Print Lisp-Style Representation of R Expression}
\usage{
showTree(e, write = cat)
}
\alias{showTree}
\arguments{
  \item{e}{R expression.}
  \item{write}{function of one argument to write the result.}
}
\description{
  Prints a Lisp-style representation of R expression.  This can be
  useful for understanding how some things are parsed.
}
\author{Luke Tierney}

\examples{
showTree(quote(-3))
showTree(quote("x"<-1))
showTree(quote("f"(x)))
}
\keyword{programming}
