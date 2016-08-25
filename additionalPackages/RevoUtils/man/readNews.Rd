\name{readNews}
\alias{readNews}
\title{Read R or Package NEWS Files}
\description{Read the R \file{NEWS} file or a package's \file{NEWS} or \file{NEWS.Rd} file.}
\usage{
readNews(package)
}
\arguments{
  \item{package}{a character string specifying the package for which you'd like to read the 
news. If missing, the R \file{NEWS} is displayed.}
}

\details{
    The R \file{NEWS} file is displayed using \code{RShowDoc} or \code{file.show}, depending
on whether the system appears capable of launching a browser. Similarly, package \file{NEWS.Rd}
files are displayed using \code{browseURL} or \code{file.show}.

	This function should not be confused with \code{\link{readNEWS}} in the \pkg{tools} package, 
which performs a different kind of reading.
}
\value{
    \code{NULL} is returned invisibly.
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
\keyword{documentation}
