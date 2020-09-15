\name{stagec}
\alias{stagec}
\docType{data}
\title{Stage C Prostate Cancer}
\description{A set of 146 patients with stage C prostate cancer, from a
  study exploring the prognostic value of flow cytometry.}
\usage{data(stagec)}
\format{
  A data frame with 146 observations on the following 8 variables.
  \describe{
    \item{\code{pgtime}}{Time to progression or last follow-up (years)}
    \item{\code{pgstat}}{1 = progression observed, 0 = censored}
    \item{\code{age}}{age in years}
    \item{\code{eet}}{early endocrine therapy, 1 = no, 2 = yes}
    \item{\code{g2}}{percent of cells in G2 phase, as found by flow cytometry}
    \item{\code{grade}}{grade of the tumor, Farrow system}
    \item{\code{gleason}}{grade of the tumor, Gleason system}
    \item{\code{ploidy}}{the ploidy status of the tumor, from flow
      cytometry. Values are \samp{diploid}, \samp{tetraploid},
      and \samp{aneuploid}}
  }
}
\details{
A tumor is called diploid (normal complement of dividing cells) if the
fraction of cells in G2 phase was determined to be 13\% or less.
Aneuploid cells have a measurable fraction with a chromosome count that
is neither 24 nor 48, for these the G2 percent is difficult or
impossible to measure.
}
\examples{
require(survival)
rpart(Surv(pgtime, pgstat) ~ ., stagec)
}
\keyword{datasets}
