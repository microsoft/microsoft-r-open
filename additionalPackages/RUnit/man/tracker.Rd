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

%%  $Id: tracker.Rd,v 1.15 2009/11/04 17:06:56 burgerm Exp $

\encoding{latin1}
\name{tracker}
\alias{tracker}

\title{Tracking the results of the inspect process.}


\usage{tracker()
}

\description{
  The current implementation uses the 'closure trick' to hide all details from
  the user and only allows to retrieve the results of the code inspection.
  \code{tracker} is used to create a new environment to manage and store the results
  of the tracking process. The \code{inspect} function requires such an
  environment with the name "track" (currently mandatory). The tracker records how often each
  and every function was called by \code{inspect} and summarizes the results of
  all calls.
  \code{tracker$init} initializes the tracker environment.
  \code{tracker$getTrackInfo} returns a list with the tracked results of the
  inspection process.
}

\section{Methods}{
  \tabular{rll}{
    \tab \code{init} \tab initializes the tracker environment\cr
    \tab \code{addFunc} \tab add function to the inspect tracking list
    (internal use)\cr
    \tab \code{getSource} \tab return the modified source code used for during inspection 
    the specified index (internal use)\cr
    \tab \code{bp} \tab update tracking info for specified function
    index (internal use)\cr
    \tab \code{getTrackInfo} \tab return 'trackInfo' object\cr
    \tab \code{isValid} \tab check 'trackInfo' object for conformance to
    class contract \cr
  }
}

\details{The 'trackInfo' S3 class object (list) has one entry for each
  function on the inspect list with the following elements:
  \describe{
  \item{src}{The source code of the function.}
  \item{run}{The number of executions for each line of code.}
  \item{graph}{A matrix. Each element in the matrix counts how often a
    code line was called from the previous code line in the execution flow.}
  \item{nrRuns}{Counts how often the function was called.}
  \item{funcCall}{The declaration of the function.}
  }
}

\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{\&} Matthias Burger}

\seealso{
  \code{\link{inspect}} for the registration of functions \& methods to
  be on the tracking list, and 
  \code{\link{printHTML.trackInfo}} for displaying results
}

\examples{

## example functions
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

bar <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y - i
   }
   return(y)
}



##  the object name track is 'fixed' (current implementation)
track <- tracker()

##  initialize the tracker
track$init()

##  inspect the function
##  resFoo1 will contain the result of calling foo(50)
resFoo1 <- inspect(foo(50), track = track)

resFoo2 <- inspect(foo(20), track = track)

resBar1 <- inspect(bar(30), track = track)

##  get the tracked function call info for all inspect calls
resTrack <- track$getTrackInfo()

##  create HTML sites in folder ./results for all inspect calls
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
