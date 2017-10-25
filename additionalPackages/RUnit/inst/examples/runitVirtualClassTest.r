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

##  $Id: runitVirtualClassTest.r,v 1.3 2009/11/04 16:54:13 burgerm Exp $


##  example code for test cases for S4 virtual class methods
##  the following class definition code would be part of a package or script
##
##  execute these test cases via e.g.
##  testSuite <- defineTestSuite("VirtualClassTest",
##                               file.path(yourSrcPath, "RUnit/inst/examples"),
##                               "runitVirtual")
##  testData <- runTestSuite(testSuite)
##  printTextProtocol(testData)


##  package 'methods' is usually loaded, but make sure it is
checkTrue(require(methods))
##  define class
className <- "MyVirtualBaseClass"
setClass(className,
           representation("VIRTUAL",
                          x = "numeric",
                          y = "numeric",
                          description = "character"),
           validity = NULL,
           sealed   = FALSE,
           where    = .GlobalEnv)

if (!isGeneric("getX")) {
  setGeneric("getX", function(object, ...) standardGeneric("getX"),
             useAsDefault=TRUE, where=.GlobalEnv, valueClass="numeric")
}
setMethod("getX", signature=className, function(object) return(object@x),
          where=.GlobalEnv)

if (!isGeneric("setX<-")) {
  setGeneric("setX<-", function(object, value) standardGeneric("setX<-"),
             useAsDefault=TRUE, where=.GlobalEnv)
}

 
setMethod("setX<-", signature=signature(object=className, value="numeric"),
          function(object, value) {
            if (length(value) < 1) {
              stop("value has to contain at least one element.")
            }
            if (any(is.na(value))) {
              stop("value may not contain NA(s).")
            }
            object@x <- value
            return(object)
          }, where=.GlobalEnv)


testMyVirtualBaseClass.getX <- function() {
  ##@bdescr
  ##  create a derived class with no own method definitions
  ##  which inherits parent class methods that can then be checked
  ##
  ##  getter test case
  ##@edescr
  
  testClassName <- "MyDerivedTestClass"
  setClass(testClassName,
           representation("MyVirtualBaseClass"),
           validity = NULL,
           sealed   = FALSE,
           where    = .GlobalEnv)
  
  on.exit(removeClass(testClassName, where=.GlobalEnv))
    
  ##  system constructor
  this <- new(testClassName)

  ##  constructor call succeeded?
  checkTrue( is(this, testClassName))

  ret <- getX(this)
  checkTrue( is(ret, "numeric"))
  ##  class default
  checkEquals( ret, numeric(0))
}


testMyVirtualBaseClass.setX <- function() {
  ##@bdescr
  ##  setter test case
  ##  also check correct handling of invalid arguments 
  ##@edescr

  testClassName <- "MyDerivedTestClass"
  setClass(testClassName,
           representation("MyVirtualBaseClass"),
           validity = NULL,
           sealed   = FALSE,
           where    = .GlobalEnv)
  
  on.exit(removeClass(testClassName, where=.GlobalEnv))
  
  ##  system constructor
  this <- new(testClassName)

  ##  constructor call succeeded?
  checkTrue( is(this, testClassName))
  
  testSeq <- 1:23
  setX(this) <- testSeq
  ret <- getX(this)
  checkTrue( is(ret, "numeric"))
  checkEquals( ret, testSeq)
  
  
  ##  error handling
  checkException( setX(this) <- numeric(0))
  checkException( setX(this) <- as.numeric(NA))
  checkException( setX(this) <- c(1:4, NA))
}
