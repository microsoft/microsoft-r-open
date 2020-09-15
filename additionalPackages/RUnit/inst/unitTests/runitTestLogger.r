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
##  $Id: runitTestLogger.r,v 1.1 2009/11/05 18:24:38 burgerm Exp $


cat("\n\nRUnit test cases for '.testLogger' object\n\n")



testRUnit..testLogger <- function() {
  ##@bdescr
  ##  test case for .testLogger object of class: 'TestLogger'
  ##@edescr

  
  ##  create new temp test logger object
  tlTmp <- RUnit:::.newTestLogger(TRUE)

  checkTrue( is(tlTmp, "TestLogger"))
  
  checkTrue( all( c("getTestData", "setCurrentTestSuite" , "setCurrentSourceFile",
                    "addSuccess", "addError", "addFailure", "addDeactivated",
                    "addCheckNum", "isFailure", "setFailure",
                    "isDeactivated", "setDeactivated",
                    "incrementCheckNum", "getCheckNum", "cleanup") %in% names(tlTmp)))

  testSuite <- defineTestSuite(name="Test Test", dirs=tempdir())
  
  ##  need to init 1) test suite name and 2) test case file name
  tlTmp$setCurrentTestSuite(testSuite)
  tlTmp$setCurrentSourceFile("Test File")
  tlTmp$addSuccess("first test case", system.time(1))
  
  checkEquals(tlTmp$getTestData()[[1]]$nTestFunc, 1)
  
  tlTmp$incrementCheckNum()
  checkEquals(tlTmp$getCheckNum(), 1)

  checkTrue( !tlTmp$isFailure())
  tlTmp$setFailure()
  checkTrue( tlTmp$isFailure())

  checkTrue( !tlTmp$isDeactivated())
  tlTmp$setDeactivated("This is a deactivated test case")
  checkTrue( tlTmp$isDeactivated())
  
  tlTmp$cleanup()
  checkEquals(tlTmp$getCheckNum(), 0)
  checkTrue( !tlTmp$isFailure())
  
}


testRUnit.getErrors <- function() {
  ##@bdescr
  ##  test case for getErrors function
  ##@edescr
  
  ##  create dummy test suite result object
  testData <- vector(mode="list", 3)
  for (i in seq_along(testData)) {
    testData[[i]]$nErr <- i
    testData[[i]]$nDeactivated <- i - 1
    testData[[i]]$nTestFunc <- i*13
    testData[[i]]$nFail <- i + 3
  }
  class(testData) <- "RUnitTestData"
         
  res <- getErrors(testData)
  checkTrue (is.list(res))
  checkEquals(length(res), 4)
  checkEquals(res$nErr, 6)
  checkEquals(res$nDeactivated, 3)
  checkEquals(res$nTestFunc, 78)
  checkEquals(res$nFail, 15)

  ##  check exception handling
  checkException( getErrors( list()))
}
