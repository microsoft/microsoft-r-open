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

##  $Id: testLogger.r,v 1.19 2009/11/05 18:54:46 burgerm Exp $


.newTestLogger <- function(useOwnErrorHandler) {
  ##@bdescr
  ##  creates a new, empty TestLogger 'object'.
  ##  TestLogger is an object based on the 'closure trick'. It has the task
  ##  to store, administrate and print the test protocol.
  ##@edescr
  ##@in  useOwnErrorHandler  : [logical] 
  ##@ret                     : [list]
  ##
  ##@codestatus : internal

  ## private data:
  ## -----------------------
  .testData <- list()
  class(.testData) <- "RUnitTestData"
  .currentTestSuiteName <- NULL
  .currentSourceFileName <- NULL

  ## book keeping variables for individual test functions
  ## can be reset by function cleanup
  .currentTraceBack <- NULL
  .failure <- FALSE
  .deactivationMsg <- NULL   ## if non-NULL test function is deactivated
  .checkNum <- 0
  ##  verbosity level:  0: silent
  .verbosity <- 1L
  
  ## define own error handler
  ## -----------------------
  errorHandler <- function() {
    ##@bdescr
    ##  used as default error handler during test case execution iff
    ##  the user specified 'useOwnErrorHandler' as TRUE (default).
    ##  called in case an error condition, typically stop() has been signalled.
    ##  tries to create a traceback object, currently only used by addError().
    ##
    ##  not provided via testLogger but used by R's error handler.
    ##@edescr
    ##
    ##@ret  : [NULL] used for it's side effect
    ##
    ##@codestatus : internal
    
    res <- try(dump.frames())
    if (inherits(res, "try-error")) {
      .currentTraceBack <<- "traceback not available (dump.frames failed)."
    } else {
      .currentTraceBack <<- names(last.dump)[-length(last.dump)]
    }
  }
  
  if(useOwnErrorHandler) {
    options(error=errorHandler)
  }



  ## public methods:
  ## -----------------------
  .getTestData <- function() {
    ##@bdescr
    ##  return the protocol data collected during the test runs
    ##@edescr
    return(.testData)
  }

  .setCurrentTestSuite <- function(testSuite) {
    ##@bdescr
    ##  record the test suite that is currently executed.
    ##@edescr
    ##@in testSuite : [testSuite - list] the current testSuite

    if(is.null(testSuite)) {
      .currentTestSuiteName <<- NULL
    } else {
      if(is.element(testSuite$name, names(.testData))) {
        stop(paste("Duplicate test suite:", testSuite$name))
      }
      .currentTestSuiteName <<- testSuite$name
      .testData[[testSuite$name]] <<- list(nTestFunc = 0L,
                                           nDeactivated = 0L,
                                           nErr  = 0,
                                           nFail = 0,
                                           dirs = testSuite[["dirs"]],
                                           testFileRegexp = testSuite[["testFileRegexp"]],
                                           testFuncRegexp = testSuite[["testFuncRegexp"]],
                                           sourceFileResults = list())
    }
  }

  .setCurrentSourceFile <- function(sourceFileName) {
    ##@bdescr
    ##  record the source file whose test functions are currently executed
    ##@edescr
    ##@in sourceFileName : [character] name of current source file

    if(is.null(sourceFileName)) {
      .currentSourceFileName <<- NULL
    } else {
      .currentSourceFileName <<- sourceFileName
      .testData[[.currentTestSuiteName]]$sourceFileResults[[sourceFileName]] <<- list()
    }
  }

  .addSuccess <- function(testFuncName, secs) {
    ##@bdescr
    ##  add a successful test function run.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in secs : [numeric] time in seconds needed by the test function to complete

    .testData[[.currentTestSuiteName]]$nTestFunc <<- 1 + .testData[[.currentTestSuiteName]]$nTestFunc

    .testData[[.currentTestSuiteName]]$sourceFileResults[[.currentSourceFileName]][[testFuncName]] <<-
      list(kind="success", checkNum=.checkNum, time=secs)
  }

  .addError <- function(testFuncName, errorMsg) {
    ##@bdescr
    ##  add a test function that generated an error.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in errorMsg : [character] the error message

    .testData[[.currentTestSuiteName]]$nTestFunc <<- 1 + .testData[[.currentTestSuiteName]]$nTestFunc
    .testData[[.currentTestSuiteName]]$nErr <<- 1 + .testData[[.currentTestSuiteName]]$nErr

    .testData[[.currentTestSuiteName]]$sourceFileResults[[.currentSourceFileName]][[testFuncName]] <<-
      list(kind="error", msg=errorMsg, checkNum=.checkNum, traceBack=.currentTraceBack)
  }

  .addFailure <- function(testFuncName, failureMsg) {
    ##@bdescr
    ##  add a test function that generated an error.
    ##@edescr
    ##@in testFuncName : [character] name of test function
    ##@in failureMsg : [character] the failure message

    .testData[[.currentTestSuiteName]]$nTestFunc <<- 1 + .testData[[.currentTestSuiteName]]$nTestFunc
    .testData[[.currentTestSuiteName]]$nFail <<- 1 + .testData[[.currentTestSuiteName]]$nFail

    .testData[[.currentTestSuiteName]]$sourceFileResults[[.currentSourceFileName]][[testFuncName]] <<-
      list(kind="failure", msg=failureMsg, checkNum=.checkNum, traceBack=NULL)  ## traceBack is useless in this case
  }

  .addDeactivated <- function(testFuncName) {
    ##@bdescr
    ##  add a deactivated test function that generated an error.
    ##@edescr
    ##@in testFuncName : [character] name of test function


    .testData[[.currentTestSuiteName]]$nDeactivated <<- 1 + .testData[[.currentTestSuiteName]]$nDeactivated
    .testData[[.currentTestSuiteName]]$sourceFileResults[[.currentSourceFileName]][[testFuncName]] <<-
      list(kind="deactivated", msg=.deactivationMsg, checkNum=.checkNum)
  }

  .addCheckNum <- function(testFuncName) {
    ##@bdescr
    ##  add total number of checks performed 
    ##@edescr
    ##@in testFuncName : [character] name of test function


    .testData[[.currentTestSuiteName]]$sourceFileResults[[.currentSourceFileName]][[testFuncName]]$checkNum <<- .checkNum
      
  }
  
  .cleanup <- function() {
    ##@bdescr
    ##  reset book keeping variables like .failure, ...
    ##  should be called before each test function execution
    ##@edescr

    .currentTraceBack <<- NULL
    .failure <<- FALSE
    .deactivationMsg <<- NULL
    .checkNum <<- 0
  }

  .isFailure <- function() {
    ##@bdescr
    ##  return current failure status 
    ##@edescr
    return(.failure)
  }

  .setFailure <- function() {
    ##@bdescr
    ##  set failure status to TRUE
    ##@edescr
    .failure <<- TRUE
  }

  .isDeactivated <- function() {
    ##@bdescr
    ##  return current deactivation message
    ##@edescr
    ##@ret  : [logical] TRUE if deactivation msg is not NULL
    return(!is.null(.deactivationMsg))
  }

  .setDeactivated <- function(msg) {
    ##@bdescr
    ##  set deactivation message variable, indicating a deactivated test case
    ##@edescr
    ##@in  msg : [character] message string
    
    if (length(msg) > 1) {
      msg <- paste(msg, collapse=" ")
    }
    .deactivationMsg <<- msg
  }

  .incrementCheckNum <- function() {
    ##@bdescr
    ##  increment internal counter of total num of test cases
    ##@edescr
    .checkNum <<- 1 + .checkNum
  }
  
  .getCheckNum <- function() {
    ##@bdescr
    ##  return counter value for total num of test cases
    ##@edescr
    return(.checkNum)
  }

  .getVerbosity <- function() {
    ##@bdescr
    ##  return verbosity level for output log messages
    ##@edescr
    return(.verbosity)
  }
  
  .setVerbosity <- function(level) {
    ##@bdescr
    ##  set verbosity level for output log messages
    ##@edescr
    ##@in  level : [integer] 0: omit output log messages, 1 >= : write begin/end comments for each test case
    
    if (length(level) > 1) {
      level <- level[1]
    }
    .verbosity <<- level
  }
  
 tl <- list(getTestData          = .getTestData,
            setCurrentTestSuite  = .setCurrentTestSuite,
            setCurrentSourceFile = .setCurrentSourceFile,
            addSuccess           = function(testFuncName, secs) .addSuccess(testFuncName, secs),
            addError             = function(testFuncName, errorMsg) .addError(testFuncName, errorMsg),
            addFailure           = function(testFuncName, failureMsg) .addFailure(testFuncName, failureMsg),
            addDeactivated       = function(testFuncName) .addDeactivated(testFuncName),
            addCheckNum          = function(testFuncName) .addCheckNum(testFuncName),
            isFailure            = .isFailure,
            setFailure           = .setFailure,
            isDeactivated        = .isDeactivated,
            setDeactivated       = function(msg) .setDeactivated(msg),
            incrementCheckNum    = .incrementCheckNum,
            getCheckNum          = .getCheckNum,
            getVerbosity         = .getVerbosity,
            setVerbosity         = .setVerbosity,
            cleanup              = .cleanup)

  class(tl) <- "TestLogger"
  return(invisible(tl))
}



getErrors <- function(testData) {
  ##@bdescr
  ##  return a brief summary of the test case execution result,
  ##  computed from the testData listOfListsOfLists
  ##
  ##@edescr
  ##
  ##@in testData : [list] S3 RUnitTestData class object
  ##@ret         : [list] containing no of errors, deactivated, failed, and total test functions
  ##
  ##@codestatus : testing
  if(class(testData) != "RUnitTestData") {
    stop("getErrors needs an object of class 'RUnitTestData' as argument.")
  }
  ret <- list(nErr=0, nDeactivated=0, nFail=0, nTestFunc=0)
  for(i in seq_along(testData)) {
    ret$nErr <- ret$nErr + testData[[i]]$nErr
    ret$nDeactivated <- ret$nDeactivated + testData[[i]]$nDeactivated
    ret$nFail <- ret$nFail + testData[[i]]$nFail
    ret$nTestFunc <- ret$nTestFunc + testData[[i]]$nTestFunc
  }
  return(ret)
}

.existsTestLogger <- function(envir=.GlobalEnv) {
  ##@bdescr
  ##  Internal Function
  ##  checks if .testLogger object is available in specified environment
  ##  and if present if this object is of class 'TestLogger'
  ##
  ##@edescr
  ##
  ##@in  envir : [environment] to search within
  ##@ret       : [logical] TRUE iff .testLogger list object is found in specified environment
  ##
  ##@codestatus : internal
  exists(".testLogger", envir=envir) && inherits(.testLogger, "TestLogger")
}
