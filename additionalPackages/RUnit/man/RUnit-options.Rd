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

%%  $Id: RUnit-options.Rd,v 1.2 2009/11/25 15:23:35 burgerm Exp $

\encoding{latin1}
\name{options}
\alias{RUnit options}
\alias{RUnit-options}
\title{RUnit options}
\description{
RUnit uses three options available via the global R options list
}

\details{
  RUnit specif options are added to R's global options list on package
  loading and removed again on pachage unloading.
}
\section{Options used in RUnit}{
  \describe{
    \item{\code{silent}:}{logical flag, default \code{FALSE}, sets the 'silent' argument for
    \code{checkException}. Allows to globally silence output from
    exception checks for all test suites excuted in one run.}
    \item{\code{verbose}:}{non-negative integer, default \code{1}, \code{0}: surpresses enclosing
    begin/end messages for each test case, \code{1}: output enclosing
    begin/end messages for each test case}
    \item{\code{outfile}:}{\code{NULL}, connection or character, default
    \code{NULL}. If non-null has to be an open connection or a file name.
    Will be used to redirect all output to specified file/connection
    using \code{sink}. Connection is close after test suite execution
    call (via \code{runTestSuite} or \code{runTestFile}) has completed.
    If the file exists it is overwriten.}
}
}
\author{Matthias Burger}

\seealso{\code{\link{options}}, \code{\link{getOption}}, \code{\link{sink}}.}

\examples{
\dontrun{
##  quiet log output
ro <- getOption("RUnit")
ro$silent <- TRUE
ro$verbose <- 0L
options("RUnit"=ro)
}
}
\keyword{programming}
\keyword{environment}
\concept{RUnit}
