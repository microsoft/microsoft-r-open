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

##  $Id: runit.r,v 1.34 2010/09/15 13:39:09 burgerm Exp $


defineTestSuite <- function(name, dirs, 
                            testFileRegexp="^runit.+\\.[rR]$",
                            testFuncRegexp="^test.+",
                            rngKind="Marsaglia-Multicarry",
                            rngNormalKind="Kinderman-Ramage")
{
  ##@bdescr
  ##  Convenience functions to handle test suites
  ##@edescr
  ##
  ##@in  name           : [character] test suite title used in protocol
  ##@in  dirs           : [character] vector of paths to search for test case files
  ##@in  testFileRegexp : [character] regular expression string to match file names
  ##@in  testFuncRegexp : [character] (vector) regular expression string(s) to match test case functions within all test case files
  ##@in  rngKind        : [character] name of the RNG version, see RNGversion()
  ##@in  rngNormalKind  : [character] name of the RNG version for the rnorm, see RNGversion()
  ##@ret                : [RUnitTestSuite] S3 class (list) object, ready for test runner
  ##
  ##@codestatus : testing

  if (missing(dirs)) {
    stop("argument 'dirs' is missing without a default.")
  }
  if (missing(name)) {
    warning("argument 'name' is missing. using basename(dirs)[1] instead.")
    name <- basename(dirs)[1]
  }
  ret <- list(name=name,
              dirs=dirs,
              testFileRegexp=testFileRegexp,
              testFuncRegexp=testFuncRegexp,
              rngKind=rngKind,
              rngNormalKind=rngNormalKind)

  class(ret) <- "RUnitTestSuite"
  return(invisible(ret))
}


isValidTestSuite <- function(testSuite)
{
  ##@bdescr
  ##  Helper function
  ##  checks 'RUnitTestSuite' class object features
  ##@edescr
  ##
  ##@in   testSuite : [RUnitTestSuite] S3 class (list) object, input object for test runner
  ##@ret            : [logical] TRUE if testSuite is valid
  ##
  ##@codestatus : testing
  
  if(!is(testSuite, "RUnitTestSuite"))
  {
    warning(paste("'testSuite' object is not of class 'RUnitTestSuite'."))
    return(FALSE)
  }
  ##  check required elements, irrespective of order, allow for additional elements
  requiredNames <-  c("name", "dirs", "testFileRegexp", "testFuncRegexp",
                      "rngKind", "rngNormalKind")
  if(!all(requiredNames %in% names(testSuite)))
  {
    warning("'testSuite' object does not conform to S3 class definition. Not all list elements present.")
    return(FALSE)
  }
  for(i in seq_along(testSuite))
  {
    if(!is.character(testSuite[[i]])) {
      warning(paste("'testSuite' object does not conform to S3 class definition.\n",
                    "'", names(testSuite)[i],"' element has to be of type 'character'.", sep=""))
      return(FALSE)
    }
    if(any(testSuite[[i]] == "")) {
      warning(paste("'testSuite' object does not conform to S3 class definition.\n",
                    "'",names(testSuite)[i],"' element may not contain empty string.", sep=""))
      return(FALSE)
    }
  }
  notFound <- !file.exists(testSuite[["dirs"]])
  if (any(notFound)) {
    warning(paste("specified directory",
                  paste(testSuite[["dirs"]][notFound], collapse=", "), "not found."))
    return(FALSE)
  }
  if (length(testSuite[["name"]]) != 1) {
    warning(paste("'name' element may only contain exactly one name."))
    return(FALSE)
  }
  if (length(testSuite[["testFileRegexp"]]) != 1) {
    warning(paste("'testFileRegexp' element may only contain exactly one string."))
    return(FALSE)
  }
  if (length(testSuite[["testFuncRegexp"]]) != 1) {
    warning(paste("'testFuncRegexp' element may only contain exactly one string."))
    return(FALSE)
  }
  ##  RNGkind has an internal list of valid names which cannot be accessed
  ##  programmatically. Furthermore, users can define their own RNG and select that one
  ##  so we have to leave it to RNGkind() to check if the arguments are valid.
  if (length(testSuite[["rngKind"]]) != 1) {
    warning(paste("'rngKind' element may only contain exactly one name."))
    return(FALSE)
  }
  if (length(testSuite[["rngNormalKind"]]) != 1) {
    warning(paste("'rngNormalKind' element may only contain exactly one name."))
    return(FALSE)
  }
  return(TRUE)
}


.setUp <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case before the test case gets executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr
  ##
  ##@codestatus : internal
  
  return(invisible())
}


.tearDown <- function() {
  ##@bdescr
  ##  Internal Function.
  ##  Default function to be executed once for each test case after the test case got executed.
  ##  This function can be adopted to specific package requirements for a given project.
  ##  Need to replace this default with a new function definition.
  ##  Function cannot take arguments and does not have a return value.
  ##@edescr
  ##
  ##@codestatus : internal
  
  return(invisible())
}


.executeTestCase <- function(funcName, envir, setUpFunc, tearDownFunc)
{
  ##@bdescr
  ##  Internal Function.
  ##  Execute individual test case, record logs and change state of global TestLogger object.
  ##@edescr
  ##
  ##@in  funcName     : [character] name of test case function
  ##@in  envir        : [environment]
  ##@in  setUpFunc    : [function]
  ##@in  tearDownFunc : [function]
  ##@ret              : [NULL]
  ##
  ##@codestatus : internal
  
  ##  write to stdout for logging

  func <- get(funcName, envir=envir)
  ## anything else than a function is ignored.
  if(mode(func) != "function") {
    return(invisible())
  }

  if (.testLogger$getVerbosity() > 0) {
    cat("\n\nExecuting test function", funcName, " ... ")
  }
  
  ## safe execution of setup function
  res <- try(setUpFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .setUp before",funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".setUp (before ", funcName, ")", sep=""),
                         errorMsg=message)
    return(invisible())
  }

  ## reset book keeping variables in .testLogger
  .testLogger$cleanup()
  ## ordinary test function execution:
  timing <- try(system.time(func()))
  if (inherits(timing, "try-error")) {
    if(.testLogger$isFailure()) {
      .testLogger$addFailure(testFuncName=funcName,
                             failureMsg=geterrmessage())
    }
    else if(.testLogger$isDeactivated()) {
      .testLogger$addDeactivated(testFuncName=funcName)
    }
    else {
      .testLogger$addError(testFuncName=funcName,
                           errorMsg=geterrmessage())
    }
  }
  else {
    .testLogger$addSuccess(testFuncName=funcName, secs=round(timing[3], 2))
  }

  ##  add number of check function calls within test case
  .testLogger$addCheckNum(testFuncName=funcName)
  
  ## safe execution of tearDown function
  res <- try(tearDownFunc())
  if (inherits(res, "try-error")) {
    message <- paste("Error executing .tearDown after",funcName, ":", geterrmessage())
    .testLogger$addError(testFuncName=paste(".tearDown (after ", funcName, ")", sep=""),
                         errorMsg=message)
    return(invisible())
  }

  if (.testLogger$getVerbosity() > 0) {
    cat(" done successfully.\n\n")
  }
  return(invisible())
}


.sourceTestFile <- function(absTestFileName, testFuncRegexp)
{
  ##@bdescr
  ## This function sources a file, finds all the test functions in it, executes them
  ## and reports the results to the TestLogger.
  ## No return value, called for its side effects on TestLogger object
  ##@edescr
  ##
  ##@in  absTestFileName : [character] absolute path name of the file to test
  ##@in  testFuncRegexp  : [character] a regular expression identifying the names of test functions
  ##@ret                 : [NULL]
  ##
  ##@codestatus : internal

  .testLogger$setCurrentSourceFile(absTestFileName)
  if (!file.exists(absTestFileName)) {
    msgText <- paste("Test case file ", absTestFileName," not found.")
    .testLogger$addError(testFuncName=absTestFileName, errorMsg=msgText)
    return(invisible())
  }
  

  sandbox <- new.env(parent=.GlobalEnv)
  ##  will be destroyed after function closure is left
  
  ##  catch syntax errors in test case file
  res <- try(sys.source(absTestFileName, envir=sandbox))
  if (inherits(res, "try-error")) {
    message <- paste("Error while sourcing ",absTestFileName,":",geterrmessage())
    .testLogger$addError(testFuncName=absTestFileName, errorMsg=message)
    return(invisible())
  }
  ##  test file provides definition of .setUp/.tearDown
  if (exists(".setUp", envir=sandbox, inherits=FALSE)) {
    .setUp <- get(".setUp", envir=sandbox)
  }
  if (exists(".tearDown", envir=sandbox, inherits=FALSE)) {
    .tearDown <- get(".tearDown", envir=sandbox)
  }
  testFunctions <- ls(pattern=testFuncRegexp, envir=sandbox)
  for (funcName in testFunctions) {
    .executeTestCase(funcName, envir=sandbox, setUpFunc=.setUp, tearDownFunc=.tearDown)
  }
  
}


runTestSuite <- function(testSuites, useOwnErrorHandler=TRUE, verbose=getOption("RUnit")$verbose) {
  ##@bdescr
  ## This is the main function of the RUnit framework. It identifies all specified
  ## test files and triggers all required actions. At the end it creates a test
  ## protocol data object. 
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specified in defineTestSuite() before each new test case *file* is sourced. 
  ## This guarantees that each new test case set defined together in on file can rely
  ## on the default, even if the random number generator version is being reconfigured in some
  ## previous test case file(s).
  ##@edescr
  ##
  ##@in  testSuites         : [list] list of test suite lists
  ##@in  useOwnErrorHandler : [logical] TRUE (default) : use the RUnit error handler
  ##@in  verbose            : [integer] >= 1: (default) write begin/end comments for each test case, 0: omit begin/end comment 
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  if (!is.logical(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' has to be of type logical.")
  }
  if (length(useOwnErrorHandler) != 1) {
    stop("argument 'useOwnErrorHandler' has to be of length 1.")
  }
  if (is.na(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' may not contain NA.")
  }

  oFile <- getOption("RUnit")$outfile
  if (!is.null(oFile)) {
    if(is.character(oFile)) {
      ##  connection has to be open when handed on to sink
      oFile <- file(oFile, "w")
    } else if(!inherits(oFile, "connection")) {
      stop("'outfile' must be a connection or a character string.")
    }
    sink(file=oFile)
    sink(file=oFile, type="message")
    resetStream <- function() {
      sink(type="message")
      sink()
      flush(oFile)
      close(oFile)
      ##close(oFile)
    }
    on.exit(resetStream())
  }
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]), add=TRUE)
  
  oldErrorHandler <- getOption("error")
  ## reinstall error handler
  on.exit(options(error=oldErrorHandler), add=TRUE)
  
  ## initialize TestLogger
  assign(".testLogger", .newTestLogger(useOwnErrorHandler), envir=.GlobalEnv)
  .testLogger$setVerbosity(verbose)
  
  ## main loop
  if (isValidTestSuite(testSuites)) {
    testSuites <- list(testSuites)
  } else if (isValidTestSuite(testSuites[[1]])) {
    ## do nothing
  } else {
    stop("invalid test suite supplied.")
  }
  for (i in seq_along(testSuites)) {
    testSuite <- testSuites[[i]]
    if(!isValidTestSuite(testSuite)) {
      errMsg <- paste("Invalid test suite",testSuite$name,". Test run aborted.")
      stop(errMsg)
    }
    .testLogger$setCurrentTestSuite(testSuite)
    testFiles <- list.files(testSuite$dirs,
                            pattern = testSuite$testFileRegexp,
                            full.names=TRUE)
    for(testFile in testFiles) {
      ## set a standard random number generator.
      RNGkind(kind=testSuite$rngKind, normal.kind=testSuite$rngNormalKind)
      
      .sourceTestFile(testFile, testSuite$testFuncRegexp)
    }
  }

  ret <- .testLogger$getTestData()
  
  return(ret)
}


runTestFile <- function(absFileName, useOwnErrorHandler=TRUE, 
                        testFuncRegexp="^test.+",
                        rngKind="Marsaglia-Multicarry",
                        rngNormalKind="Kinderman-Ramage",
                        verbose=getOption("RUnit")$verbose) {
  ##@bdescr
  ##  Convenience function.
  ##@edescr
  ##
  ##@in  absFileName        : [character] complete file name of test cases code file
  ##@in  useOwnErrorHandler : [logical] if TRUE RUnits error handler will be used
  ##@in  testFuncRegexp     : [character]
  ##@in  rngKind            : [character] name of the RNG, see RNGkind for avialbale options
  ##@in  rngNormalKind      : [character] name of the RNG for rnorm, see RNGkind for avialbale options
  ##@in  verbose            : [integer] >= 1: (default) write begin/end comments for each test case, 0: ommit begin/end comment (passed on to function runTestSuite)
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  ##  all error checking and handling is delegated to function runTestSuite
  
  fn <- basename(absFileName)
  nn <- strsplit(fn, "\\.")[[1]][1]
  dn <- dirname(absFileName)
  ts <- defineTestSuite(name=nn, dirs=dn, 
                        testFileRegexp=paste("^", fn, "$", sep=""),
                        testFuncRegexp=testFuncRegexp,
                        rngKind=rngKind,
                        rngNormalKind=rngNormalKind)
                        
  return(runTestSuite(ts, useOwnErrorHandler=useOwnErrorHandler,
                      verbose=verbose))
}
