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
##  $Id: runitTearDown.r,v 1.2 2009/04/16 09:18:52 burgerm Exp $


cat("\n\nRUnit test cases for '.tearDown' function\n\n")

##  defined for the life time of this environment
warningsLengthDefault <- getOption("warnings.length")


.tearDown <- function() {
  ##  remove global variable
  if (exists("runitDummy", envir=.GlobalEnv)) {
    try(rm("runitDummy", envir=.GlobalEnv))
  }
  
  ##  remove temp file
  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  if (file.exists(tempFile)) {
    try(unlink(tempFile))
  }
  
  ##  reset options
  options(warnings.length=warningsLengthDefault)

  ##  remove class
  if (length(findClass("runitDummyS4Class")) > 0) {
    try(removeClass("runitDummyS4Class", where=.GlobalEnv))
  }
}


testRUnit..tearDown.Init <- function() {
  ##@bdescr
  ##  testcase for function .tearDown of class: none
  ##  setup vriables to be removed by .tearDown after test case execution
  ##  check in subsequent test case that this operation chain succeeded
  ##@edescr


  ##  define global variable
  checkTrue( !exists("runitDummy", where=.GlobalEnv))
  assign("runitDummy", "this is a test dummy variable", envir=.GlobalEnv)
  checkTrue( exists("runitDummy", where=.GlobalEnv))
            
  ##  create temp file
  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  checkTrue( !file.exists(tempFile))
  write.table(matrix(1:42, 6, 7), file=tempFile)
  checkTrue( file.exists(tempFile))

  ##  modify options
  ##  current default: 1000
  options(warnings.length=123)
  checkEqualsNumeric( getOption("warnings.length"), 123)

  ##  define S4 class
  checkTrue( !isClass("runitDummyS4Class", where=.GlobalEnv))
  setClass("runitDummyS4Class",
           representation(x = "numeric",
                          y = "numeric"),
           prototype(x = 1:10,
                     y = 10:1),
           where=.GlobalEnv)
  checkTrue( isClass("runitDummyS4Class", where=.GlobalEnv))
}


testRUnit..tearDown.Test <- function() {
  ##@bdescr
  ##  testcase for function .tearDown of class: none
  ##  test that all modifications resulting from the previous test case
  ##  have been removed as defined in .tearDown
  ##@edescr


  checkTrue( !exists("runitDummy", where=.GlobalEnv))

  tempFile <- file.path(tempdir(), "runitDummyFile.txt")
  checkTrue( !file.exists(tempFile))

  checkEqualsNumeric( getOption("warnings.length"), warningsLengthDefault)
                     
  checkTrue( !isClass("runitDummyS4Class", where=.GlobalEnv))
}
