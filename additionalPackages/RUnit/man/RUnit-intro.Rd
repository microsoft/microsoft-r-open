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

%%  $Id: RUnit-intro.Rd,v 1.6 2009/11/05 19:32:48 burgerm Exp $

\encoding{latin1}
\name{RUnit}
\alias{RUnit}
\title{RUnit - Package Description}
\description{
This package models the common Unit Test framework for R
and provides functionality to track results of test case execution and
generate a summary report. It also provides tools for code inspection
and thus for test case coverage analysis.
The design is inspired by the popular JUnit unit test framework.

This package comes with a set of unit tests, serving as a test battery to check 
correct functioning against new R versions released as well as practical 
examples for writing test cases (see the \file{inst/unitTests} subdirectory of 
the source package, or \file{unitTests} contained in the 
binary package version).

The \R wiki has a section on setting up a test suite for your package and 
combining it with \command{R CMD check} as well as references to alternative 
implementations:

\url{http://wiki.r-project.org/rwiki/doku.php?id=developers:runit}
}

\references{RUnit - A Unit Test Framework for R. useR! 2004 Vienna}
\author{
Thomas \enc{König}{Koenig}, Klaus \enc{Jünemann}{Juenemann} 
\ifelse{html}{\out{&amp;}}{&} Matthias Burger}

\seealso{
See  \code{\link{defineTestSuite}}, \code{\link{runTestSuite}}
for unit testing or \code{\link{inspect}} and
\code{\link{tracker}} for code inspection.
}

\keyword{programming}
\concept{RUnit}
