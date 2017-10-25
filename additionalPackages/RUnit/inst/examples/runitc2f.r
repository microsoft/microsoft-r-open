## This is a very trivial demo  of
## the RUnit test case execution system:
## ---------------------------------

## functions to be tested (usually defined in a different
## file from where the test cases are located):

## centigrade to Fahrenheit
c2f <- function(c) return(9/5 * c + 32)

## Fahrenheit to centigrade
f2c <- function(f) return(5/9 * f - 32)  ## ups, a bug (brackets missing)



## test functions:
## ---------------------


.setUp <- function() {  ## called before each test case, see also .tearDown()
  print(".setUp")
}

test.c2f <- function() {
  checkEquals(c2f(0), 32)
  checkEquals(c2f(10), 50)
  ## check that an error is created for a bogus argument
  checkException(c2f("xx"))
}


test.f2c <- function() {
  checkEquals(f2c(32), 0)
  checkEquals(f2c(50), 10)
  ## check that an error is created for a bogus argument
  checkException(f2c("xx"))
}

test.errordemo <- function() {
  stop("this is just to show what an error looks like as opposed to a failure")
}





## How to run the tests (do not uncomment in this file,
## but execute the commands at the R prompt):
## All you have to do is to adapt the directory locations.
## ------------------------------------------------

## define the test suite:
#testsuite.cf <- defineTestSuite("cfConversion", dirs="directoryOfThisFile")

## run test suite:
#testResult <- runTestSuite(testsuite.cf)

## print text protocol to console:
#printTextProtocol(testResult)

## print HTML version to a file:
#printHTMLProtocol(testResult, fileName="someFileName.html")

## In this case we also have a shortcut
#runTestFile("directoryOfThisFile/runitcfConversion.r")
