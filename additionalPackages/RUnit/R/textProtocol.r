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

##  $Id: textProtocol.r,v 1.18 2009/11/04 17:02:44 burgerm Exp $


printTextProtocol <- function(testData,
                              fileName = "",
                              separateFailureList = TRUE,
                              showDetails = TRUE,
                              traceBackCutOff=9) {

  ##@bdescr
  ##  Report generator
  ##  Extracts the log information stored in the 'RUnitTestData' test run object
  ##  and generates a well formated protocol output.
  ##@edescr
  ##
  ##@in  testData            : [RUnitTestData] S3 class object
  ##@in  fileName            : [character] string, full path + file name to be written to
  ##@in  separateFailureList : [logical] if TRUE (default) add a failure list
  ##@in  showDetails         : [logical] if TRUE (default) add detailed traceback for each error incurred
  ##@in  traceBackCutOff     : [integer] number of steps back in the trace back stack to display
  ##@ret                     : [logical] TRUE if execution completed without error
  ##
  ##@codestatus : testing

  ##  preconditions
  if (!is(testData, "RUnitTestData")) {
    stop("Argument 'testData' must be of class 'RUnitTestData'.")
  }

  if (!is.character(fileName)) {
    stop("Argument 'fileName' has to be of type character.")
  }
  if (length(fileName) != 1) {
    stop("Argument 'fileName' must contain exactly one element.")
  }

  if (!is.logical(separateFailureList)) {
    stop("Argument 'separateFailureList' has to be of type logical.")
  }
  if (length(separateFailureList) != 1) {
    stop("Argument 'separateFailureList' must contain exactly one element.")
  }

  if (!is.logical(showDetails)) {
    stop("Argument 'showDetails' has to be of type logical.")
  }
  if (length(showDetails) != 1) {
    stop("Argument 'showDetails' must contain exactly one element.")
  }

  if (!is.numeric(traceBackCutOff)) {
    stop("Argument 'traceBackCutOff' has to be of type logical.")
  }
  if (length(traceBackCutOff) != 1) {
    stop("Argument 'traceBackCutOff' must contain exactly one element.")
  }
  if (traceBackCutOff < 0 || traceBackCutOff > 100) {
    stop("Argument 'traceBackCutOff' out of valid range [0, 100].")
  }


  ## just a convenience function
  pr <- function(..., sep=" ", nl=TRUE) {
    if(nl) {
      cat(... , "\n", file = fileName, append=TRUE, sep=sep)
    } else {
      cat(... , file = fileName, append=TRUE, sep=sep)
    }
  }

  ## get singular or plural right
  sop <- function(number, word, plext="s") {
    ifelse(number == 1, paste(number, word),
           paste(number, paste(word, plext, sep="")))
  }

  ## header part
  cat("RUNIT TEST PROTOCOL --", date(), "\n", file = fileName)
  pr("***********************************************")
  if(length(testData) == 0) {
    pr("no test cases :-(")
    return(invisible(TRUE))
  }

  errInfo <- getErrors(testData)
  pr("Number of test functions:", errInfo$nTestFunc)
  if(errInfo$nDeactivated > 0) {
    pr("Number of deactivated test functions:", errInfo$nDeactivated)
  }
  pr("Number of errors:", errInfo$nErr)
  pr("Number of failures:", errInfo$nFail, "\n\n")


  ## summary of test suites
  pr(sop(length(testData), "Test Suite"), ":")
  for(tsName in names(testData)) {
    pr(tsName, " - ", sop(testData[[tsName]]$nTestFunc, "test function"), ", ",
       sop(testData[[tsName]]$nErr, "error"), ", ",
       sop(testData[[tsName]]$nFail, "failure"), sep="")
    if(separateFailureList && (testData[[tsName]]$nErr + testData[[tsName]]$nFail > 0)) {
      srcFileRes <- testData[[tsName]][["sourceFileResults"]]
      for(i in seq_along(srcFileRes)) {
        testFuncNames <- names(srcFileRes[[i]])
        for(j in seq_along(testFuncNames)) {
          funcList <- srcFileRes[[i]][[testFuncNames[j]]]
          if(funcList$kind == "error") {
            pr("ERROR in ", testFuncNames[j], ": ", funcList$msg, nl=FALSE, sep="")
          } else if(funcList$kind == "failure") {
            pr("FAILURE in ", testFuncNames[j], ": ", funcList$msg,
               sep="", nl=FALSE)
          } else if(funcList$kind == "deactivated") {
            pr("DEACTIVATED ", testFuncNames[j], ": ", funcList$msg,
               sep="", nl=FALSE)
          }
        }
      }
    }
  }


  ## if no details are required, we are done.
  if(!showDetails) {
    return(invisible(TRUE))
  }
  
  pr("\n\n\nDetails")

  ## loop over all test suites
  for(tsName in names(testData)) {
    tsList <- testData[[tsName]]
    pr("***************************")
    pr("Test Suite:", tsName)
    pr("Test function regexp:", tsList$testFuncRegexp)
    pr("Test file regexp:", tsList$testFileRegexp)
    if(length(tsList$dirs) == 0) {
      pr("No directories !")
    } else {
      if(length(tsList$dirs) == 1) {
        pr("Involved directory:")
      } else {
        pr("Involved directories:")
      }
      for(dir in tsList$dirs) {
        pr(dir)
      }
      res <- tsList$sourceFileResults
      testFileNames <- names(res)
      if(length(res) == 0) {
        pr("no test files")
      } else {
        ## loop over all source files
        for(testFileName in testFileNames) {
          testFuncNames <- names(res[[testFileName]])
          if(length(testFuncNames) > 0) {
            pr("---------------------------")
            pr("Test file:", testFileName)
            ## loop over all test functions in the test file
            for(testFuncName in testFuncNames) {
              testFuncInfo <- res[[testFileName]][[testFuncName]]
              if(testFuncInfo$kind == "success") {
                pr(testFuncName, ": (",testFuncInfo$checkNum, " checks) ... OK (", 
                   testFuncInfo$time, " seconds)", sep="")
              } else {
                if(testFuncInfo$kind == "error") {
                  pr(testFuncName, ": ERROR !! ", sep="")
                } else if (testFuncInfo$kind == "failure") {
                  pr(testFuncName, ": FAILURE !! (check number ", 
                     testFuncInfo$checkNum, ")", sep="")
                } else if (testFuncInfo$kind == "deactivated") {
                  pr(testFuncName, ": DEACTIVATED, ", nl=FALSE)
                } else {
                  pr(testFuncName, ": unknown error kind", sep="")
                }
                pr(testFuncInfo$msg, nl=FALSE)
                if(length(testFuncInfo$traceBack) > 0) {
                  pr("   Call Stack:")
                  if(traceBackCutOff > length(testFuncInfo$traceBack)) {
                    pr("   (traceBackCutOff argument larger than length of ",
                       "trace back: full trace back printed)")
                    for(i in 1:length(testFuncInfo$traceBack)) {
                      pr("   ", i, ": ", testFuncInfo$traceBack[i], sep="")
                    }
                  } else {
                    for(i in traceBackCutOff:length(testFuncInfo$traceBack)) {
                      pr("   ", 1+i-traceBackCutOff, ": ", 
                         testFuncInfo$traceBack[i], sep="")
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  ##  return type  
  return(invisible(TRUE))
}


print.RUnitTestData <- function(x, ...)
{
  ##@bdescr
  ##  Generic print method
  ##@edescr
  ##
  ##@in  x   : [RUnitTestData] S3 class object
  ##@in  ... : [ANY] currently ignored
  ##@ret     : [NULL]
  ##
  ##@codestatus : untested
  
  errInfo <- getErrors(x)
  cat("Number of test functions:", errInfo$nTestFunc, "\n")
  if(errInfo$nDeactivated > 0) {
    cat("Number of deactivated test functions:", errInfo$nDeactivated, "\n")
  }
  cat("Number of errors:", errInfo$nErr, "\n")
  cat("Number of failures:", errInfo$nFail, "\n")
  
}


summary.RUnitTestData <- function(object, ...)
{

  ##@bdescr
  ##  Generic summary method
  ##@edescr
  ##
  ##@in  object : [RUnitTestData] S3 class object
  ##@in  ...    : [ANY]
  ##@ret        : [logical] return valof from printTextProtocol
  ##
  ##@codestatus : untested
  
  printTextProtocol(object, ...)
}

