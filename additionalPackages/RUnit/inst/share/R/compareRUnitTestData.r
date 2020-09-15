######################################################################
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

##  $Id: compareRUnitTestData.r,v 1.4 2009/11/04 16:52:47 burgerm Exp $

compare <- function(td1, td2, tolerance=100) {
  ##@bdescr
  ##  compare two test suite result data objects obtained
  ##  on the same test suite
  ##  identify timing differences exceeding 'tolerance' [seconds]
  ##@edescr
  ##
  ##
  ##@in  td1       : [list] of S3 class 'RUnitTestData'
  ##@in  td2       : [list] of S3 class 'RUnitTestData'
  ##@in  tolerance : [numeric] positive scalar
  ##@ret           : [data.frame]
  ##
  ##@codestatus : untested

  ##  preconditions
  if (!is(td1, "RUnitTestData")) {
    stop("argument 'td1' has to be of class 'RUnitTestData'.")
  }
  if (!is(td2, "RUnitTestData")) {
    stop("argument 'td2' has to be of class 'RUnitTestData'.")
  }
  if (length(tolerance) != 1 || is.na(tolerance) || tolerance < 0) {
    stop("argument 'tolerance' has to be positive scalar.")
  }

  ##  helper functions
  commonNames <- function(x1, x2) {
    return(intersect(names(x1), names(x2)))
  }
  compareTiming <- function(x1, x2, tol=0) {
    d <- x1 - x2
    if (abs(d) > tol) {
      return(d)
    } else {
      return(as.numeric(0))
    }
  }
 
  comparePerSourceFile <- function(sf1,sf2, tol=0) {
    ##  FIXME
    ##  check if test case file was considered in this suite
    ##  i.e. test case file name can be in list albeit it
    ##  was not executed in the scenario
    ##  thus list is empty
    if (length(sf1) == 0 | length(sf2) == 0) {
      cat("\n  skipped empty result set:", sf1)
      return(NULL)
    }
    commonTests <- commonNames(sf1, sf2)
    t(sapply(commonTests, function(x, obj1, obj2) {
      ##cat("\n test:", x, "\n")
      
      if(obj1[[x]][["kind"]] == obj2[[x]][["kind"]]) {
        if (obj1[[x]][["kind"]] == "success") {
          return(c(x, obj1[[x]][["kind"]], obj1[[x]][["time"]],
                   obj2[[x]][["kind"]], obj2[[x]][["time"]],
                   compareTiming(obj1[[x]][["time"]],
                                 obj2[[x]][["time"]],
                                 tol=tol)))
        } else {
          return(c(x, obj1[[x]][["kind"]], as.numeric(NA),
                   obj2[[x]][["kind"]], as.numeric(NA), as.numeric(NA)))
        }
      } else {
        ##  no timing delta
        ##  should check for timing in second case
        ##  obj2[[x]][["time"]])
        return(c(x, obj1[[x]][["kind"]], as.numeric(NA),
                 obj2[[x]][["kind"]], as.numeric(NA), as.numeric(NA)))

      }
      
    }, obj1=sf1, obj2=sf2))
  }
  
  comparePerSuite <- function(s1,s2, tol=0) {
    ##  absolute file names recorded, strip path
    commonFiles <- intersect(basename(names(s1[["sourceFileResults"]])),
                             basename(names(s2[["sourceFileResults"]])))
    do.call("rbind", sapply(commonFiles, function(x, obj1, obj2) {
      ##  match exact file name in abs. name
      idx1 <- match(x, basename(names(obj1)))
      idx2 <- match(x, basename(names(obj2)))
      if (length(idx1) != 1 || is.na(idx1) || length(idx2) != 1 || is.na(idx2)) {
        stop("ambiguous file name.")
        next;
      }
      comparePerSourceFile(obj1[[idx1]], obj2[[idx2]], tol=tol)
    }, obj1=s1[["sourceFileResults"]], obj2=s2[["sourceFileResults"]]) )

  }
           

  ##  main
  ##  test suites to compare
  commonTestSuites <- commonNames(td1, td2)

  res <- matrix(ncol=6, nrow=0)
  colnames(res) <- c("TestCase", "Suite1 State", "Suite1 Timing", "Suite2 State", "Suite2 Timing", "Delta")
  for (ti in seq_along(commonTestSuites)) {
    res <- rbind(res, comparePerSuite(td1[[commonTestSuites[ti]]], td2[[commonTestSuites[ti]]],
                                      tol=tolerance))
  }
  
  ##  postcondition
  return(res)
}

