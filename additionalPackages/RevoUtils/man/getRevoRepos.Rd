\name{getRevoRepos}
\alias{getRevoRepos}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Path to Revolution Analytics Package Repositories
}
\description{
\code{getRevoRepos} returns the path to either the Revolution Analytics CRAN mirror or the
versioned package source repository maintained by Revolution Analytics.
}
\usage{
getRevoRepos(version = paste(unlist(unclass(getRversion()))[1:2], collapse = "."), CRANmirror = FALSE)
}
\arguments{
  \item{version}{
A string or floating-point literal representing an R version from 2.9 or later. (2.10 is a valid version,
but the versioned package source repository does not exist for this version of R.) This argument is ignored
if \code{CRANmirror=TRUE}.
}
  \item{CRANmirror}{
A logical flag. If \code{TRUE}, the path to the Revolution Analytics CRAN mirror is returned.
}
}
\value{
A character string containing the path to the versioned package source repository
or the Revolution Analytics CRAN mirror.
}

\author{
Microsoft Corporation \email{mrspack@microsoft.com}
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
getRevoRepos("2.13")

getRevoRepos(2.12)

getRevoRepos(CRANmirror=TRUE)
}
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
