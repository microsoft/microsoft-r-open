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
##  $Id: runitSetUp.r,v 1.2 2009/04/16 09:18:52 burgerm Exp $


cat("\n\nRUnit test cases for '.setUp' function\n\n")

##  defined for the life time of this environment
warningsLengthDefault <- getOption("warnings.length")


.setUp <- function() {

  ##  define global variable
  if (exists("runitDummy", where=.GlobalEnv)) {
    stop(".setUp found 'runitDummy'.")
  }
  assign("runitDummy", "this is a test dummy variable", envir=.GlobalEnv)
  if( !exists("runitDummy", where=.GlobalEnv)) {
    stop(".setUp failed to assign 'runitDummy'.")
  }
            
  ##  create temp file
  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  checkTrue( !file.exists(tempFile))
  write.table(matrix(1:42, 6, 7), file=tempFile)
  if ( !file.exists(tempFile)) {
    stop(paste(".setUp failed to create file", tempFile))
  }

  ##  modify options
  ##  current default: 1000
  options(warnings.length=123)
  if ( !identical(TRUE, all.equal.numeric(getOption("warnings.length"), 123))) {
     stop(paste(".setUp failed to set options."))
  }

  ##  define S4 class
  checkTrue( !isClass("runitDummyS4Class", where=.GlobalEnv))
  setClass("runitDummyS4Class",
           representation(x = "numeric",
                          y = "numeric"),
           prototype(x = 1:10,
                     y = 10:1),
           where=.GlobalEnv)
  if ( !isClass("runitDummyS4Class", where=.GlobalEnv)) {
    stop(paste(".setUp failed to define S4 class 'runitDummyS4Class'."))
  }
  
}


testRUnit..setUp.Test1 <- function() {
  ##@bdescr
  ##  testcase for function .setUp of class: none
  ##  check existance of variables, modified options, class definitions
  ##  defined in .setUp
  ##  remove for subsequent check
  ##@edescr

  checkTrue( exists("runitDummy", where=.GlobalEnv))
  ##  remove global variable
  rm("runitDummy", envir=.GlobalEnv)
  
  
  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  checkTrue( file.exists(tempFile))
  ##  remove temp file
  unlink(tempFile)
  

  checkEqualsNumeric( getOption("warnings.length"), 123)
  ##  reset options
  options(warnings.length=warningsLengthDefault)

  checkTrue( isClass("runitDummyS4Class", where=.GlobalEnv))
  ##  remove class
  checkTrue( removeClass("runitDummyS4Class", where=.GlobalEnv))
}


testRUnit..setUp.Test2 <- function() {
  ##@bdescr
  ##  testcase for function .setUp of class: none
  ##   same as above, only reason is to check correct invocation of .setUp
  ##   before *each* test case function
  ##@edescr


  checkTrue( exists("runitDummy", where=.GlobalEnv))
  ##  remove global variable
  rm("runitDummy", envir=.GlobalEnv)
  
  
  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  checkTrue( file.exists(tempFile))
  ##  remove temp file
  unlink(tempFile)
  

  checkEqualsNumeric( getOption("warnings.length"), 123)
  ##  reset options
  options(warnings.length=warningsLengthDefault)

  checkTrue( isClass("runitDummyS4Class", where=.GlobalEnv))
  ##  remove class
  checkTrue( removeClass("runitDummyS4Class", where=.GlobalEnv))
}
