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

%%  $Id: printHTML.trackinfo.Rd,v 1.13 2009/11/05 19:46:28 burgerm Exp $

\encoding{latin1}
\name{printHTML.trackInfo}
\alias{printHTML.trackInfo}
\alias{printHTML}
\title{Write HTML pages of the tracking result.}

\usage{
  printHTML.trackInfo(object, baseDir = ".")
}

\arguments{
  \item{object}{'trackInfo' S3 class object (list), containing the result of the
   function \code{tracker}.}
  \item{baseDir}{A character string, specifying the base directory for
  the HTML pages to be written to. Defaults to the current working directory.}
}

\description{
  \code{printHTML.trackInfo} creates a subdirectory named
  "result"  in the base directory specified via \code{baseDir}. All HTML pages 
  and images will be put
  in that directory.
}

\details{An "index.html" page will be created in the directory
  "results" which is the root entry page of the HTML pages. The
  displayed result for every tracked function consists of two HTML pages.
  The first page is an overview on how
  often every line of code was executed. Code lines not executed are
  highlighted red, executed lines are shown in green. The second page is
  a graph representation of the
  execution flow of the function. Each code line has a edge pointing to
  the next code line that is executed subsequently. Thus loops and jumps
  become clearly visible.
}

\author{Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
  \code{\link{tracker}} for the call tracking object definition.
}
\examples{

##  example function
foo <- function(x){
   y <- 0
   for(i in 1:100)
   {
      y <- y + i
   }
   return(y)
}

##  the name track is necessary
track <- tracker()

##  initialize the tracker
track$init()

##  inspect the function
##  res is the result of foo
res <- inspect(foo(10), track = track)

##  get the tracking info
resTrack <- track$getTrackInfo()

##  create HTML pages
printHTML.trackInfo(resTrack)
}

\keyword{programming}
\concept{RUnit}
