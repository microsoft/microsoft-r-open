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
##
##  $Id: runitRUnit.r,v 1.18 2010/09/15 13:40:25 burgerm Exp $


cat("\n\nRUnit test cases for 'RUnit:check' functions\n\n")


testRUnit.checkEquals <- function()
{
  ##@bdescr
  ## test case for function checkEquals of class: none
  ##@edescr

  ##  integer
  x <- 1:10
  checkEquals(x, x)
  ##  return value
  checkTrue( checkEquals(x, x))
  namedInt <- 1:10
  names(namedInt) <- letters[namedInt]
  checkEquals(namedInt, namedInt)
  checkEquals(namedInt, x, checkNames=FALSE)
  
  ##  numeric
   checkEquals(pi, pi)
  y <- 1/0
  checkEquals(Inf, y)
  checkEquals(y, Inf)
  y <- log(-1)
  checkEquals(NaN, y)
  checkEquals(rep(NaN, 23), rep(y, 23))
  checkEquals(9, 9.0)
  checkEquals(NA, NA)
  checkEquals(rep(NA, 14), rep(NA, 14))
  checkEquals( numeric(1), numeric(1))
  checkEquals( 0.01, 0.02, tolerance=0.01)
  tmp <- c(0.01, NA, 0.02, Inf, -Inf, NaN, 1.0)
  checkEquals( tmp, tmp, tolerance=0.01)
  
  ##  complex
  checkEquals(complex(0), complex(0))
  checkEquals(complex(2), complex(2))
  checkEquals(complex(2, imaginary=1), complex(2, imaginary=1))

  ##  character
  checkEquals( character(1), character(1))
  checkEquals( letters, letters)
  
  ##  matrix
  checkEquals( matrix(1, 3,5), matrix(1, 3,5))
  checkEquals( matrix(1, 50000,5), matrix(1, 50000,5),
              "large matrix not identified as equal")

  ##  language
  checkEquals( expression(2), expression(2))
  checkEquals( call("mean", "median"), call("mean", "median"))

  ##  formula
  simpleForm <- x ~ 1
  checkEquals( simpleForm, simpleForm,
              "simple formula not identified as equal")
  compForm <- y ~ x + y + x*y + offset(x)
  checkEquals( compForm, compForm,
              "formula not identified as equal")
  
  ##  factor
  alphaFac <- factor(letters)
  checkEquals( alphaFac, alphaFac,
              "factor not identified as equal")
  
  ##  list
  checkEquals( list(100), list(100))
  checkEquals( list(100), list(100), tolerance=1)
  alphaList <- seq_along(letters)
  names(alphaList) <- letters
  checkEquals( alphaList, alphaList)
  checkEquals( alphaList, alphaList, checkNames=FALSE)

  ##  nested list with NA, NaN, Inf
  nl <- list(a=list(1), b=list(1:4),
             c=list(ab=1, bc=list(list(2), list(NA), list(NaN)) ),
             d=list(m1=matrix(NA, 2,3), m2=matrix(1+1i, 4,5)),
             e=list(e1=NaN, e2=list(Inf), e3=list(a=Inf, b=-Inf, c=NaN, d=-0/0)))
  checkEquals(nl, nl)
  
  ##  example from ?glm
  counts <- c(18,17,15,20,10,20,25,13,12)
  outcome <- gl(3,1,9)
  treatment <- gl(3,3)
  lmFit <- glm(counts ~ outcome + treatment, family=poisson())
  checkEquals( lmFit, lmFit, checkNames=FALSE)
  checkEquals( lmFit, lmFit)
  lmFitUnnamed <- lmFit
  names(lmFitUnnamed) <- NULL
  checkEquals( lmFit, lmFitUnnamed, checkNames=FALSE)
  
  ##  POSIXct
  sysTime <- as.POSIXct(Sys.time())
  checkEquals( sysTime, sysTime)

  ##  raw
  checkEquals( raw(14), raw(14))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkEquals( namedRaw, namedRaw)

  ##  formula
  a <- 1:10
  f <- gl(2,5)
  checkEquals( a~f, a~f)
  
  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track1",
             representation(x="numeric", y="numeric"),
             where=.GlobalEnv)
    on.exit(removeClass("track1", where=.GlobalEnv))
    
    s4Obj <- try(new("track1"))
    s4Obj@x <- 1:10
    s4Obj@y <- 10:1
    checkEquals( s4Obj, s4Obj)

    ##  S4 class containing S4 class slot
    setClass("trackPair",
             representation(trackx = "track1",
                            tracky = "track1"),
             where=.GlobalEnv)
    
    on.exit(removeClass("trackPair", where=.GlobalEnv), add=TRUE)

    tPair <- new("trackPair")
    tPair@trackx <- s4Obj
    tPair@tracky <- s4Obj
    checkEquals( tPair, tPair)
  }

  if (require(Biobase)) {
    ##   class still available?
    #if (isClass(Class="ExpressionSet", formal=TRUE)) {
    #  ES <- new("ExpressionSet", exprs=matrix(runif(1000), nrow=100, ncol=10))
    #  checkEquals(ES, ES)
    #}
    ##  cleanup workspace
    ##  catch error if this ns is required by some other package
    ##  and therefore cannot be unloaded
    try(unloadNamespace("Biobase"))
  }

  
  ##  detect differences
  checkException( checkEquals(1 , 1, tolerance=FALSE))
  checkException( checkEquals(1 , 1, tolerance=numeric(0)))
  checkException( checkEquals(1 , 1, tolerance=numeric(2)))

  ##  integer
  namedInt <- 1:9
  names(namedInt) <- letters[namedInt]
  checkException( checkEquals( namedInt, 1:9))
  
  ##  numeric
  checkException( checkEquals( 8, 9))
  checkException( checkEquals( 0.01, 0.02, tolerance=0.009))
  checkException(checkEquals(NaN, NA))
  checkException(checkEquals(NaN, Inf))
  checkException(checkEquals(NaN, -Inf))
  checkException(checkEquals(NA, Inf))
  checkException(checkEquals(NA, -Inf))
  checkException(checkEquals(numeric(2), numeric(3)))
  checkException(checkEquals(numeric(3), numeric(2)))
  
  ##  complex
  checkException( checkEquals(complex(0), complex(1)))
  checkException( checkEquals(complex(2), complex(1)))
  checkException( checkEquals(complex(2, imaginary=1), complex(2, imaginary=0)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=1, imaginary=0)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=1)))
  checkException( checkEquals(complex(2, real=1, imaginary=1), complex(2, real=0, imaginary=0)))
  
  ##  character
  named <- character(1)
  names(named) <- "name"
  checkException( checkEquals( character(1), named))
  checkException( checkEquals( letters, letters[-1]))
  
  ##  formula
  checkException( checkEquals( lmFit, lmFitUnnamed))
  lmFitInter <- glm(counts ~ outcome * treatment, family=poisson())
  checkException( checkEquals( lmFitInter, lmFit))
  
  ##  factor
  alphaFacRecoded <- factor(alphaFac, labels=as.character(seq_along(levels(alphaFac))))
  checkException( checkEquals(alphaFacRecoded, alphaFac))
  
  ##  list
  checkException( checkEquals( list(1), list("1"=1)))
  checkException( checkEquals( list(), list("1"=1)))
  checkException( checkEquals( list(list(), list(list()), list(list(list()))),
                              list(list(), list(list()), list(list(list(), list())))))

  
  ##  POSIXct
  checkException( checkEquals(as.POSIXct(Sys.time()), as.POSIXct("2007-04-04 16:00:00")))
  checkException( checkEquals(as.POSIXlt(Sys.time()), as.POSIXlt("2007-04-04 16:00:00")))
  
  ##  nested type
  sysTime <- as.POSIXct(Sys.time())
  checkException( checkEquals( list(a=2, list(time=sysTime)), list(a=2, time=list(sysTime))))

  ##  raw
  checkException( checkEquals(raw(1), raw(2)))
  checkException( checkEquals(raw(1E5), raw(100001)))
  raw3 <- raw(3)
  raw3mod <- raw3
  raw3mod[1] <- as.raw(3)
  checkException( checkEquals(raw3, raw3mod))
  checkException( checkEquals(as.raw(1:1000), as.raw(c(1:99,-1,101:1000)) ) )

  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    ##  class defined above
    s4Obj <- new("track1")
    s4Obj@x <- 1:10
    checkException( checkEquals( s4Obj, new("track1")))

    tPair <- new("trackPair")
    tPair@trackx <- s4Obj
    checkException( checkEquals( tPair, new("trackPair")))
  }

}


testRUnit.checkEqualsNumeric <- function()
{
  ##@bdescr
  ## test case for function checkEqualsNumeric of class: none
  ##@edescr

  checkTrue( checkEqualsNumeric( 9,9))
  checkTrue( checkEqualsNumeric( 9.1,9.2, tolerance=0.1))
  x <- 1:10
  attributes(x) <- list(dummy="nonsense")
  checkTrue( checkEqualsNumeric( x, x))
  checkTrue( checkEqualsNumeric( 1:10, x, check.attributes=FALSE))

  
  rvec <- rnorm(132)
  checkTrue( checkEqualsNumeric( matrix(rvec, 12, 11), matrix(rvec, 12, 11)))
  checkTrue( checkEqualsNumeric( rvec, rvec))

  ##  special constants
  checkEqualsNumeric( pi, pi)
  checkEqualsNumeric( NA, NA)
  checkEqualsNumeric( c(1, NA, 3), c(1, NA, 3))
  checkEqualsNumeric( NaN, NaN)
  checkEqualsNumeric( c(1, NaN, 3), c(1, NaN, 3))
  checkEqualsNumeric( Inf, Inf)
  checkEqualsNumeric( c(1, Inf, 3), c(1, Inf, 3))
  checkEqualsNumeric( -Inf, -Inf)
  checkEqualsNumeric( c(1, -Inf, 3), c(1, -Inf, 3))

  
  ##  numeric difference
  checkException( checkEqualsNumeric( 9, 10))
  checkException( checkEqualsNumeric( list(9), list(10)))
  checkException( checkEqualsNumeric( matrix(9), matrix(10)))
  rvec2 <- rnorm(132)
  checkException( checkEqualsNumeric( matrix(rvec, 12, 11), matrix(rvec2, 12, 11)))

  
  ##  exception handling
  ##  type not supported
  checkException( checkEqualsNumeric( list(rvec), list(rvec)))
  if (require(Biobase)) {

    ##   class still available?
    if (isClass(Class="ExpressionSet", formal=TRUE)) {
      ES <- new("ExpressionSet", exprs=matrix(runif(1000), nrow=100, ncol=10))
      checkException(checkEqualsNumeric(ES, ES))
    }
    ##  cleanup workspace
    try(unloadNamespace("Biobase"))
  }

}


testRUnit.checkIdentical <- function()
{
  ##@bdescr
  ## test case for function checkIdentical of class: none
  ##@edescr

  checkIdentical( TRUE, TRUE)
  ##  return value
  checkTrue( checkIdentical( TRUE, TRUE))
  checkIdentical( FALSE, FALSE)

  ##  bit representation identical
  checkIdentical( NA, NA)
  checkIdentical( c(1, NA, 3), c(1, NA, 3))
  checkIdentical( NaN, NaN)
  checkIdentical( c(1, NaN, 3), c(1, NaN, 3))
  checkIdentical( Inf, Inf)
  checkIdentical( c(1, Inf, 3), c(1, Inf, 3))
  checkIdentical( -Inf, -Inf)
  checkIdentical( c(1, -Inf, 3), c(1, -Inf, 3))
  
  checkIdentical( as.integer(2), as.integer(2))
  checkIdentical( as.character(2), as.character(2))
  checkIdentical( as.complex(2), as.complex(2))
  checkIdentical( as.numeric(2), as.numeric(2))
  checkIdentical( as.expression("2+4"), as.expression("2+4"))
  checkIdentical( as.expression(2+4), as.expression(2+4))
  checkIdentical( as.factor(letters), factor(letters))
  
  ##  nested list with NA, NaN, Inf
  nl <- list(a=list(1), b=list(1:4),
             c=list(ab=1, bc=list(list(2), list(NA), list(NaN)) ),
             d=list(m1=matrix(NA, 2,3), m2=matrix(1+1i, 4,5)),
             e=list(e1=NaN, e2=list(Inf), e3=list(a=Inf, b=-Inf, c=NaN, d=-0/0)))
  checkIdentical(nl, nl)

  ##  POSIX
  sysTime <- as.POSIXlt(Sys.time())
  checkIdentical( sysTime, sysTime)

  ##  raw
  checkIdentical( raw(14), raw(14))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkIdentical( namedRaw, namedRaw)

  ##  formula
  a <- 1:10
  f <- gl(2,5)
  checkIdentical( a~f, a~f)

  ##  call
  cl <- call("round", 10.5)
  checkIdentical( cl, cl)

  
  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
  ctl <- c(4.17,5.58,5.18,6.11,4.50,4.61,5.17,4.53,5.33,5.14)
  trt <- c(4.81,4.17,4.41,3.59,5.87,3.83,6.03,4.89,4.32,4.69)
  group <- gl(2,10,20, labels=c("Ctl","Trt"))
  weight <- c(ctl, trt)
  lm.D9 <- lm(weight ~ group)
  checkIdentical( lm.D9, lm(weight ~ group))


  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track1",
             representation(x="numeric", y="numeric"),
             where=.GlobalEnv)
    on.exit(removeClass("track1", where=.GlobalEnv))

    s4Obj <- try(new("track1"))
    checkIdentical( s4Obj, new("track1"))
    rm(s4Obj)
  }

  
  ##  exception handling
  ##  type mismatches
  checkException( checkIdentical( as.integer(2), as.numeric(2)))
  checkException( checkIdentical( as.integer(2), as.character(2)))
  checkException( checkIdentical( as.integer(2), as.list(2)))
  checkException( checkIdentical( as.integer(2), as.complex(2)))
  checkException( checkIdentical( as.integer(2), as.expression(2)))

  ##  value mismatches
  checkException( checkIdentical( as.integer(2), as.integer(3)))
  checkException( checkIdentical( as.character(2), as.character(3)))
  checkException( checkIdentical( as.complex(2), as.complex(3)))
  checkException( checkIdentical( as.numeric(2), as.numeric(3)))
  checkException( checkIdentical( as.expression("2+4"), as.expression("2+3")))
  checkException( checkIdentical( as.factor(letters), factor(letters[-1])))
  fac <- factor(letters)
  levels(fac) <- c("1", letters[-1])
  checkException( checkIdentical( fac,  as.factor(letters)))
  
  ##  nested list with NA, NaN, Inf
  checkException( checkIdentical( ))

  
  ##  POSIX
  sysTime <- as.POSIXlt(Sys.time())
  checkException( checkIdentical( sysTime, as.POSIXlt(Sys.time(), tz="GMT")))
  
  ##  raw
  checkException(checkIdentical( raw(14), raw(13)))
  namedRaw <-  as.raw(1:14)
  names(namedRaw) <- letters[1:14]
  checkException(checkIdentical( namedRaw, as.raw(1:14)))
                 
  ##  S3 objects (ie. lists with attributes)
  ##  from ?lm Example
  lm.D9base <- lm(weight ~ group - 1)
  checkException( checkIdentical( lm.D9base, lm.D9))

  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track2",
             representation(x="numeric", y="numeric"),
             prototype(x=as.numeric(1:23), y=as.numeric(23:1)),
             where=.GlobalEnv)
    on.exit(removeClass("track2", where=.GlobalEnv), add=TRUE)

    s4Obj <- try(new("track2"))
    s4ObjDiff <- s4Obj
    s4ObjDiff@y <- s4ObjDiff@x
    checkException( checkIdentical( s4Obj, s4ObjDiff))
  }

}


testRUnit.checkTrue <- function()
{
  ##@bdescr
  ## test case for function checkTrue of class: none
  ##@edescr


  checkEquals( checkTrue( TRUE), TRUE)

  ##  named arguments
  namedArg <- TRUE
  names(namedArg) <- "Yes"
  checkEquals( checkTrue( namedArg), TRUE)


  ##  errorr handling
  namedArg <- FALSE
  names(namedArg) <- "No"
  checkException( checkTrue( namedArg))
  
  checkException( checkTrue( FALSE))
  
  ##  incorrect length
  checkException( checkTrue( c(TRUE, TRUE)))
  checkException( checkTrue( c(FALSE, TRUE)))
  checkException( checkTrue( logical(0)))
  checkException( checkTrue( logical(2)))
  
}


testRUnit.checkException <- function()
{
  ##@bdescr
  ## test case for function checkException of class: none
  ##@edescr

  checkException( checkTrue( FALSE))
  checkException( checkTrue( ))
  checkException( checkEquals( ))
  checkException( checkEquals( 24))
  checkException( checkEquals( 24, 24, tolerance="dummy"))
  checkException( checkEqualsNumeric( ))
  checkException( checkEqualsNumeric( 24))
  checkException( checkEqualsNumeric( 24, 24, tolerance="dummy"))

  checkException( stop("with message"), silent=FALSE)
  checkException( stop("wo message"), silent=TRUE)

  ##  R 2.5.0 devel example that failed
  ##  minimal example provided by Seth Falcon
  ll = list()
  ll[[1]] = function(x) stop("died")
  checkException( do.call(ll[[1]], list(1)))

  ##  S4 objects
  if (identical(TRUE, require(methods))) {
    setClass("track2",
             representation(x="numeric", y="numeric"),
             prototype(x=as.numeric(1:23), y=as.numeric(23:1)),
             where=.GlobalEnv)
    on.exit(removeClass("track2", where=.GlobalEnv))

    s4Obj <- try(new("track2"))
    checkException( slot(s4Obj, "z"))
    checkException( slot(s4Obj, "z") <- 1:10)
  
    ##  missing method argument
    ##  coerce(from, to)
    checkException( coerce(s4Obj))
  }
  
}


testRUnit.DEACTIVATED <- function()
{
  ##@bdescr
  ## test case for function DEACTIVATED of class: none
  ##@edescr

  checkException( DEACTIVATED())
  checkException( DEACTIVATED("some message"))
  ##  compound text
  checkException( DEACTIVATED(c("some message", "some more", "and more")))
}


testRUnit.defineTestSuite <- function()
{
  ##@bdescr
  ## test case for function defineTestSuite of class: none
  ##@edescr
  
  ##  correct working
  testSuite <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"), 
                               testFileRegexp="correctTestCase.r")
  
  ##  this also works for S3 objects
  checkTrue( inherits(testSuite, "RUnitTestSuite"))
  checkTrue( is.list(testSuite))
  checkTrue( all(c("name", "dirs", "testFileRegexp", "testFuncRegexp",
                   "rngKind", "rngNormalKind") %in% names(testSuite)))
  checkTrue( isValidTestSuite(testSuite))
  
  
  ##  error handling
  ##  missing 'dirs' argument
  checkException(defineTestSuite("RUnit Example", testFileRegexp="correctTestCase.r"))
}


testRUnit.isValidTestSuite <- function()
{
  ##@bdescr
  ## test case for function isValidTestSuite of class: none
  ##@edescr
  
  ##  correct working
  testSuite <- defineTestSuite("RUnit Example",
                               system.file("examples", package="RUnit"),
                               testFileRegexp="correctTestCase.r")
  checkTrue( isValidTestSuite(testSuite))
  
  ##  error handling
  ##  has to be S3 class 'RUnitTestSuite'
  testSuiteFail <- testSuite
  class(testSuiteFail) <- "NotUnitTestSuite"
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  ##  expecting list elements
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- NULL
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  ##  has to be character
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
 
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  
  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- list()
  checkTrue( !isValidTestSuite(testSuiteFail))
  

  ##  length 1 required
  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["name"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["testFileRegexp"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["testFuncRegexp"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["rngKind"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- character(0)
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["rngNormalKind"]] <- character(2)
  checkTrue( !isValidTestSuite(testSuiteFail))

  ##  director has to exist
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- "doesNotExist"
  checkTrue( !isValidTestSuite(testSuiteFail))

  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- c(tempdir(), "doesNotExist", tempdir())
  checkTrue( !isValidTestSuite(testSuiteFail))

  ##  same, '' has to return FALSE
  testSuiteFail <- testSuite
  testSuiteFail[["dirs"]] <- c(tempdir(), "", tempdir())
  checkTrue( !isValidTestSuite(testSuiteFail))
 
}
  

testRUnit.runTestFile <- function()
{
  ##@bdescr
  ## test case for function runTestFile of class: none
  ##@edescr

  testFile <- file.path(system.file("examples", package="RUnit"), "correctTestCase.r")
  checkTrue( file.exists(testFile))

  ## The issue: .testLogger is the hard coded logger object
  ## regenerated by each new run call in the global environment
  ## thereby overwriting the existing logger.
  ## With the current implementation there seems to be no way to
  ## test the test suite execution *within* a test suite run
  
#  tmpFile <- tempfile()
#  writeLines(text=" testFile <- file.path(system.file(\"examples\", package=\"RUnit\"), \"correctTestCase.r\");\n res <- runTestFile(testFile, useOwnErrorHandler=FALSE);\n", con=tmpFile)
#
#  execEnv <- new.env(parent=.GlobalEnv)
#  sys.source(tmpFile, execEnv)
#  unlink(tmpFile)
  
#  checkTrue(exists("res", envir=execEnv))
#  checkTrue(inherits(get("res", envir=execEnv), "RUnitTestData"))
#  rm(execEnv)
  
  ##  error handling
  ##  all argument checks delegated to runTestSuite so no need for comprehensive check here
  ##  check if any argument check is reached/performed
  ##  useOwnErrorHandler
  ##  type logical
  checkException( runTestFile(testFile, useOwnErrorHandler=integer(1)))
}


testRUnit.runTestSuite <- function()
{
  ##@bdescr
  ## test case for function runTestSuite of class: none
  ##@edescr

  testSuiteTest <- defineTestSuite("RUnit Example", system.file("examples", package="RUnit"),
                                   testFileRegexp="correctTestCase.r")

  checkTrue( isValidTestSuite(testSuiteTest))

  ## The issue: same as above
  ##res <- runTestSuite(testSuiteTest)
  ##
  
  
  ##  error handling
  ##
  ##  useOwnErrorHandler
  ##  type logical
  tS <- testSuiteTest
  checkException( runTestSuite(tS, useOwnErrorHandler=integer(1)))
  ##  length 1
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(0)))
  checkException( runTestSuite(tS, useOwnErrorHandler=logical(2)))
  checkException( runTestSuite(tS, useOwnErrorHandler=as.logical(NA)))
  
  ##  testSuite
  

}
