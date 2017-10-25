##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2009  Thomas Koenig, Matthias Burger, Klaus Juenemann
##
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; version 2 of the License.
##
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
##
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

##  $Id: checkFuncs.r,v 1.24 2009/11/05 18:57:56 burgerm Exp $


checkEquals <- function(target, current, msg="",
                        tolerance = .Machine$double.eps^0.5, checkNames=TRUE, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal'
  ## with tolerance one can adjust to and allow for numerical imprecision

  ##@edescr
  ##@in  target    : [ANY] one thing to be compared
  ##@in  current   : [ANY] second object to be compared
  ##@in  msg       : [character] an optional message to further identify and document the call
  ##@in  tolerance : [numeric] directly passed to 'all.equal', see there for further documentation
  ##@in  checkNames: [logical] iff TRUE do not strip names attributes from current and target prior to the comparison
  ##@ret           : [logical] TRUE iff check was correct
  ##
  ##@codestatus : testing

  if (missing(current)) {
     stop("argument 'current' is missing")
  }
  if(!is.numeric(tolerance)) {
    stop("'tolerance' has to be a numeric value")
  }
  if (length(tolerance) != 1) {
    stop("'tolerance' has to be a scalar")
  }
  if(!is.logical(checkNames)) {
    stop("'checkNames' has to be a logical value")
  }
  if (length(checkNames) != 1) {
    stop("'checkNames' has to be a scalar")
  }
  if(.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }
  if (!identical(TRUE, checkNames)) {
    names(target)  <- NULL
    names(current) <- NULL
  }
  result <- all.equal(target, current, tolerance=tolerance, ...)
  if (!identical(result, TRUE)) {
    if(.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop(paste(result, collapse="\n"), msg)
  } else {
    return(TRUE)
  }
}


checkEqualsNumeric <- function(target, current, msg="", tolerance = .Machine$double.eps^0.5, ...)
{
  ##@bdescr
  ## checks if two objects are equal, thin wrapper around 'all.equal.numeric'
  ## with tolerance one can adjust to and allow for numerical imprecision.
  ## current and target are converted via as.vector() thereby stripping all attributes.
  ##@edescr
  ##@in target    : [ANY] one thing to be compared
  ##@in current   : [ANY] second object to be compared
  ##@in tolerance : [numeric] directly passed to 'all.equal.numeric', see there for further documentation
  ##@in msg       : [character] an optional message to further identify and document the call
  ##
  ##@ret          : [logical] TRUE, if objects 'target' and 'current' are equal w.r.t. specified numerical tolerance, else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(current)) {
    stop("argument 'current' is missing")
  }
  if(!is.numeric(tolerance)) {
    stop("'tolerance' has to be a numeric value")
  }
  if (length(tolerance) != 1) {
    stop("'tolerance' has to be a scalar")
  }
  if(.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }
  ##  R 2.3.0: changed behaviour of all.equal
  ##  strip attributes before comparing current and target
  result <- all.equal.numeric(as.vector(target), as.vector(current), tolerance=tolerance, ...)
  if (!identical(result, TRUE)) {
    if(.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop(paste(result, collapse="\n"), msg)
  } else {
    return(TRUE)
  }
}


checkIdentical <- function(target, current, msg="")
{
  ##@bdescr
  ## checks if two objects are exactly identical, thin convenience wrapper around 'identical'
  ##
  ##@edescr
  ##@in target   : [ANY] one object to be compared
  ##@in current  : [ANY] second object to be compared
  ##@in msg      : [character] an optional message to further identify and document the call
  ##
  ##@ret         : [logical] TRUE, if objects 'target' and 'current' are identical
  ##
  ##@codestatus : testing

  if (missing(current)) {
    stop("argument 'current' is missing")
  }
  if(.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }
  
  result <- identical(target, current)
  if (!identical(TRUE, result)) {
    if(.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop(paste(paste(result, collapse="\n"), msg))
  } else {
    return(TRUE)
  }
}


checkTrue <- function(expr, msg="")
{
  ##@bdescr
  ## checks whether or not something is true
  ##@edescr
  ##@in expr : [expression] the logical expression to be checked to be TRUE
  ##@in msg  : [character] optional message to further identify and document the call
  ##
  ##@ret     : [logical] TRUE, if the expression in a evaluates to TRUE, else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(expr)) {
    stop("'expr' is missing")
  }
  if(.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }

  ##  allow named logical argument 'expr'
  result <- eval(expr)
  names(result) <- NULL
  
  if (!identical(result, TRUE)) {
    if(.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop("Test not TRUE\n", msg)
  } else {
    return(TRUE)
  }
}


checkException <- function(expr, msg="", silent=getOption("RUnit")$silent)
{
  ##@bdescr
  ## checks if a function call creates an error. The passed function must be parameterless.
  ## If you want to check a function with arguments, call it like this:
  ## 'checkException(function() func(args...))'
  ##
  ##  adding argument silent was suggested by Seth Falcon <sfalcon@fhcrc.org>
  ##  who provided a patch.
  ##@edescr
  ##@in  expr   : [parameterless function] the function to be checked
  ##@in  msg    : [character] an optional message to further identify and document the call
  ##@in  silent : [logical] passed on to try, iff TRUE error messages will be suppressed 
  ##
  ##@ret        : [logical] TRUE, if evaluation of the expression results in a 'try-error', else a stop signal is issued 
  ##
  ##@codestatus : testing

  if (missing(expr)) {
    stop("'expr' is missing")
  }
  if(is.null(silent)) {
    silent <- FALSE
    warning("'silent' has to be of type 'logical'. Was NULL. Set to FALSE.")
  }
  if(.existsTestLogger()) {
    .testLogger$incrementCheckNum()
  }

  if (!inherits(try(eval(expr, envir=parent.frame()), silent=silent), "try-error")) {
    if(.existsTestLogger()) {
      .testLogger$setFailure()
    }
    stop("Error not generated as expected\n", msg)
  } else {
    return(TRUE)
  }
}


DEACTIVATED <- function(msg="")
{
  ##@bdescr
  ##  Convenience function, for maintaining test suites.
  ##  If placed in an existing test case call
  ##  the test will be executed normally until occurrence of the call
  ##  after which execution will leave the test case (so all code will
  ##  be checked and errors or failures reported as usual).
  ##  An entry for a separate table in the log will be added
  ##  for this test case.
  ##
  ##@edescr
  ##@in msg : [character] optional message to further identify and document the call
  ##
  ##@codestatus : testing

  if(.existsTestLogger()) {
    .testLogger$setDeactivated(paste(msg, "\n", sep=""))
  }
  stop(msg)
}
