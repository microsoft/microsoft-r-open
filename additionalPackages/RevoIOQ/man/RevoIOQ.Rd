\name{RevoIOQ}
\alias{RevoIOQ}
\title{Microsoft R Services Quality Assurance}
\description{Installation and operation qualification of Microsoft R Services}

\usage{
RevoIOQ(printText=TRUE, printHTML=TRUE, outdir=if (file.access(getwd(), mode=2)) file.path(tempdir(),"RevoIOQ") else file.path(getwd(),"RevoIOQ"), 
        basename=paste("Revo-IOQ-Report", format(Sys.time(), "\%m-\%d-\%Y-\%H-\%M-\%S"), sep="-"),
        view=TRUE, clean=TRUE, runTestFileInOwnProcess=TRUE, testLocal = FALSE, testScaleR = TRUE)
}
\arguments{
  \item{printText}{logical flag. If \code{TRUE}, an RUnit test report in ASCII text format is produced.}
  \item{printHTML}{logical flag. If \code{TRUE}, an RUnit test report in HTML format is produced.}
  \item{outdir}{character string representing path to output report directory.}
  \item{basename}{character string denoting the name of the output file (sans the extension).}
  \item{view}{logical flag. If \code{TRUE} and either \code{printText=TRUE} or \code{printHTML=TRUE}, then 
  the resulting output file is shown, respectively.}   
  \item{clean}{logical flag. If \code{TRUE}, then graphics files created during the tests are removed.}
  \item{runTestFileInOwnProcess}{logical flag. If \code{TRUE}, each test file 
  is run in a separate R process.}
  \item{testLocal}{logical flag. If \code{TRUE}, a set of basic tests is created and run for all locally-installed packages.}
  \item{testScaleR}{logical flag. If \code{TRUE}, the RevoScaleR unit tests are included in the test suite, if available.}
}

\value{
    \code{TRUE} if all tests are successful, \code{FALSE} otherwise.
}

\examples{
RevoIOQ()
}

\keyword{package}
