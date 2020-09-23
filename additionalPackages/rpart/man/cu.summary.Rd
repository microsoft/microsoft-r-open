\name{cu.summary}
\alias{cu.summary}
\title{Automobile Data from 'Consumer Reports' 1990}
\description{
  The \code{cu.summary} data frame has 117 rows and 5 columns,
  giving data on makes of cars taken from the April, 1990 issue of
  \emph{Consumer Reports}.
}
\usage{
cu.summary
}
\format{
  This data frame contains the following columns:
  \describe{
    \item{\code{Price}}{
      a numeric vector giving the list price in US dollars of a standard model
    }
    \item{\code{Country}}{
      of origin, a factor with levels
      \samp{Brazil},
      \samp{England},
      \samp{France},
      \samp{Germany},
      \samp{Japan},
      \samp{Japan/USA},
      \samp{Korea},
      \samp{Mexico},
      \samp{Sweden} and
      \samp{USA}
    }
    \item{\code{Reliability}}{
      an ordered factor with levels
      \samp{Much worse} < \samp{worse} < \samp{average} < \samp{better} < \samp{Much better}
    }
    \item{\code{Mileage}}{
      fuel consumption miles per US gallon, as tested.
    }
    \item{\code{Type}}{
      a factor with levels
      \code{Compact}
      \code{Large}
      \code{Medium}
      \code{Small}
      \code{Sporty}
      \code{Van}
    }
  }
}
\source{
  \emph{Consumer Reports}, April, 1990, pp. 235--288 quoted in

  John M. Chambers and Trevor J. Hastie eds. (1992)
  \emph{Statistical Models in S}, Wadsworth and Brooks/Cole,
  Pacific Grove, CA, pp. 46--47.
}
\seealso{
  \code{\link{car.test.frame}},
  \code{\link{car90}}
}
\examples{
fit <- rpart(Price ~ Mileage + Type + Country, cu.summary)
par(xpd = TRUE)
plot(fit, compress = TRUE)
text(fit, use.n = TRUE)
}
\keyword{datasets}
