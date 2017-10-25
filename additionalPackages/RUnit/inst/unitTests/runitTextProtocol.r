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
##  $Id: runitTextProtocol.r,v 1.3 2009/04/16 09:18:52 burgerm Exp $


cat("\n\nRUnit test cases for 'textProtocol' function\n\n")


testRUnit.printTextProtocol <- function()
{
  ##  copy baseenv() logger
  tmp <- get(".testLogger", envir = .GlobalEnv)
  testCaseDir <- file.path(system.file(package="RUnit"), "examples")
  testSuiteInternal <- defineTestSuite("RUnit Self Test", testCaseDir, "correctTestCase.r")
  testData2 <- runTestSuite(testSuiteInternal, useOwnErrorHandler=FALSE)

  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  testProtocolFile <- file.path(tempdir(), paste(timeStamp, "test_printHTMLProtocol.txt", sep="_"))
  ret <- printTextProtocol(testData2, fileName=testProtocolFile)

  assign(".testLogger", tmp, envir = .GlobalEnv)
  
  checkTrue( file.exists(testProtocolFile))


  ##  input argument error handling
  ##  missing 'testData' object
  checkException(printTextProtocol())

  ##  wrong class
  checkException(printTextProtocol("dummy"))

  
  ##  fileName arg errors
  testData <- list()
  class(testData) <- "RUnitTestData"
  ##  wrong type
  checkException(printTextProtocol(testData, fileName=numeric(1)))
  ##  wrong length
  checkException(printTextProtocol(testData, fileName=character(0)))
  checkException(printTextProtocol(testData, fileName=character(2)))

  
  ##  separateFailureList arg errors
  ##  wrong type
  checkException(printTextProtocol(testData, separateFailureList=numeric(0)))
  ##  wrong length
  checkException(printTextProtocol(testData, separateFailureList=logical(0)))
  checkException(printTextProtocol(testData, separateFailureList=logical(2)))

  ##  showDetails arg errors
  ##  wrong type
  checkException(printTextProtocol(testData, showDetails=numeric(0)))
  ##  wrong length
  checkException(printTextProtocol(testData, showDetails=logical(0)))
  checkException(printTextProtocol(testData, showDetails=logical(2)))
}

