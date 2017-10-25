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

%%  $Id: testCaseSetUp.Rd,v 1.2 2009/11/05 20:12:04 burgerm Exp $

\encoding{latin1}
\name{.setUp}
\alias{.setUp}
\alias{.tearDown}
\title{Definition of RUnit Test Case code files.}

\usage{
.setUp()
.tearDown()
}

\description{
  Either one or both functions have to be provided by the test case author, take
  precedence over the dummy definitions provided by the RUnit
  package and are called once for every test case identified.
}

\details{
  To be written ...
 
}

\value{
    Functions do not return a value; called for their side effects.
}
    
\author{
Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
  \code{\link{runTestFile}}.
}

\keyword{programming}
\concept{RUnit}
