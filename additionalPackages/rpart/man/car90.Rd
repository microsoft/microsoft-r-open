\name{car90}
\alias{car90}
\docType{data}
\title{Automobile Data from 'Consumer Reports' 1990}
\description{
  Data on 111 cars, taken from pages 235--255, 281--285 and 287--288 of the
  April 1990 \emph{Consumer Reports} Magazine.
}
\usage{data(car90)}
\format{  
  The data frame contains the following columns
  \describe{
    \item{Country}{a factor giving the country in which the car was
      manufactured}
    \item{Disp}{engine displacement in cubic inches}
    \item{Disp2}{engine displacement in liters}
    \item{Eng.Rev}{engine revolutions per mile, or engine speed at 60 mph}
    \item{Front.Hd}{distance between the car's head-liner and the head
      of a 5 ft. 9 in. front seat passenger, in inches, as measured by CU}
    \item{Frt.Leg.Room}{maximum front leg room, in inches, as measured by CU}
    \item{Frt.Shld}{front shoulder room, in inches, as measured by CU}
    \item{Gear.Ratio}{the overall gear ratio, high gear, for manual transmission}
    \item{Gear2}{the overall gear ratio, high gear, for automatic transmission}
    \item{HP}{net horsepower}
    \item{HP.revs}{the red line---the maximum safe engine speed in rpm}
    \item{Height}{height of car, in inches, as supplied by manufacturer}
    \item{Length}{overall length, in inches, as supplied by manufacturer}
    \item{Luggage}{luggage space}
    \item{Mileage}{a numeric vector of gas mileage in miles/gallon as
      tested by CU; contains NAs.}
    \item{Model2}{alternate name, if the car was sold under two labels}
    \item{Price}{list price with standard equipment, in dollars}
    \item{Rear.Hd}{distance between the car's head-liner and the head of
      a 5 ft 9 in. rear seat passenger, in inches, as measured by CU}
    \item{Rear.Seating}{rear fore-and-aft seating room, in inches,
      as measured by CU}
    \item{RearShld}{rear shoulder room, in inches, as measured by CU}
    \item{Reliability}{an ordered factor with levels \samp{Much worse} <
      \samp{worse} < \samp{average} < \samp{better} < \samp{Much
	better}: contains \code{NA}s.}
    \item{Rim}{factor giving the rim size}
    \item{Sratio.m}{Number of turns of the steering
      wheel required for a turn of 30 foot radius, manual steering}
    \item{Sratio.p}{Number of turns of the steering
      wheel required for a turn of 30 foot radius, power steering}
    \item{Steering}{steering type offered: manual, power, or both}
    \item{Tank}{fuel refill capacity in gallons}
    \item{Tires}{factor giving tire size}
    \item{Trans1}{manual transmission, a factor with levels \samp{},
      \samp{man.4}, \samp{man.5} and \samp{man.6}}
    \item{Trans2}{automatic transmission, a factor with levels \samp{},
      \samp{auto.3}, \samp{auto.4}, and \samp{auto.CVT}.  No car is
      missing both the manual and automatic transmission variables, but
      several had both as options} 
    \item{Turning}{the radius of the turning circle in feet}
    \item{Type}{a factor giving the general type of car.
      The levels are: \samp{Small}, \samp{Sporty}, \samp{Compact}, \samp{Medium}, \samp{Large}, \samp{Van}}
    \item{Weight}{an order statistic giving the relative weights of the
      cars; 1 is the lightest and 111 is the heaviest}
    \item{Wheel.base}{length of wheelbase, in inches, as supplied by manufacturer}
    \item{Width}{width of car, in inches, as supplied by manufacturer}

}}
\source{
  This is derived (with permission) from the data set \code{car.all} in
  S-PLUS, but with some further clean up of variable names and definitions.
}
\seealso{
  \code{\link{car.test.frame}},
  \code{\link{cu.summary}} for extracts from other versions of the dataset.
}

\examples{
data(car90)
plot(car90$Price/1000, car90$Weight,
     xlab = "Price (thousands)", ylab = "Weight (lbs)")
mlowess <- function(x, y, ...) {
    keep <- !(is.na(x) | is.na(y))
    lowess(x[keep], y[keep], ...)
}
with(car90, lines(mlowess(Price/1000, Weight, f = 0.5)))
}
\keyword{datasets}
