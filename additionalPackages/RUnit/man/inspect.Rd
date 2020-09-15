%%  RUnit : A unit test framework for the R programming language
%%  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
%%
%%  This program is free software; you can redistribute it and/or modify
%%  it under the terms of the GNU General Public License as published by
%%  the Free Software Foundation; version 2 of the License.
%%
%%  This program is distributed in the hope that it will be useful,
%%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%  GNU General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program; if not, write to the Free Software
%%  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

%%  $Id: inspect.Rd,v 1.11 2009/11/05 19:46:28 burgerm Exp $

\encoding{latin1}
\name{inspect}
\alias{inspect}
\title{Track the executed code lines of a function or method.}

\usage{
  inspect(expr, track = track)
}

\arguments{
  \item{expr}{Any R function or method call.}
  \item{track}{list object, as returned by a call to \code{tracker}.}
}

\description{
  \code{inspect} examines and modifies the source code of a function or
  method.  After the modification of the source code, the modified function
  will be executed and the result of the tracking process will be
  stored. To store the information a \code{tracker} environment with the
  name track must exist.
  Note, that not all R code constructs can be handled at the current
  state. In some cases it is not possible to track a specific code line.
  Therefore, clearly structured code with consequent use of
  opening and closing braces to indicate conditional expressions can
  prevent these parser problems.
}

\details{The return value of \code{inspect} is the result returned by
the function executed. If the function has no return value nothing is
returned either.}

\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
  \code{\link{tracker}} for the call tracking object, and 
  \code{\link{printHTML.trackInfo}} for displaying results.
}

\examples{

## example function
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

## the name track is necessary
track <- tracker()

## initialize the tracker
track$init()

## inspect the function
## res will collect the result of calling foo
res <- inspect(foo(10), track = track)

## get the tracked function call info
resTrack <- track$getTrackInfo()

## create HTML sites
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
