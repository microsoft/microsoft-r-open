\name{solder.balance}
\alias{solder.balance}
\alias{solder}
\title{Soldering of Components on Printed-Circuit Boards}
\description{
  The \code{solder.balance} data frame has 720 rows and 6 columns, representing
  a balanced subset of a designed experiment varying 5 factors on the
  soldering of components on printed-circuit boards.

  The \code{solder} data frame is the full version of the data with 900
  rows.  It is located in both the rpart and the survival packages.
}
\usage{
solder
}
\format{
This data frame contains the following columns:
\describe{
\item{\code{Opening}}{
a factor with levels
\samp{L}, \samp{M} and \samp{S}
indicating the amount of clearance around the mounting pad.
}
\item{\code{Solder}}{
a factor with levels
\samp{Thick} and \samp{Thin}
giving the thickness of the solder used.
}
\item{\code{Mask}}{
a factor with levels
\samp{A1.5}, \samp{A3}, \samp{B3} and \samp{B6}
indicating the type and thickness of mask used.
}
\item{\code{PadType}}{
a factor with levels \samp{D4}, \samp{D6}, \samp{D7}, \samp{L4},
\samp{L6}, \samp{L7}, \samp{L8}, \samp{L9}, \samp{W4} and \samp{W9}
giving the size and geometry of the mounting pad.
}
\item{\code{Panel}}{
 \code{1:3} indicating the panel on a board being tested.
}
\item{\code{skips}}{
a numeric vector giving the number of visible solder skips.
}
}}
\source{
   John M. Chambers and Trevor J. Hastie eds. (1992)
  \emph{Statistical Models in S}, Wadsworth and Brooks/Cole,
  Pacific Grove, CA.
}
\examples{
fit <- rpart(skips ~ Opening + Solder + Mask + PadType + Panel,
             data = solder.balance, method = "anova")
summary(residuals(fit))
plot(predict(fit), residuals(fit))
}
\keyword{datasets}
