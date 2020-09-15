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

##  $Id: checkCode.r,v 1.3 2009/11/04 16:48:23 burgerm Exp $


checkCodeFiles <- function(fileName) {
  ##@bdescr
  ##  utility function for code checks of files outside usual R/ folder structure
  ##  requires package codetools
  ##@edescr
  ##
  ##@in  fileName : [character] vector of file names (including path, relative to pwd or absolute)
  ##@ret          : [list] with elements per function, that incurred any warning
  ##
  ##@depends : codetools
  ##
  ##@codestatus : untested

  
  ##  return list
  retList <- list()
  
  ##  initialized before ls() call to avoid listing
  ok <- x <- c()
  tmpRetEnv <- new.env()
  tmpRet <- NULL
  
  ##  generate listing of existing objects before first source'ing
  lsTmp <- lsInit <- ls()
  
  sapply(fileName, function(x) {
    cat("\n file ",x)
    
    ok <- try(utils::capture.output(source(x, local=TRUE, echo=FALSE)))
    if (inherits(ok, "try-error")) {
      cat("\n file",x,"could not be sourced:", geterrmessage(), "\n")
      return()
    }
    newElements <- setdiff(ls(), lsTmp)
    cat("\n  functions",paste(newElements, collapse=", "))
    lsTmp <- ls()
    sapply(newElements, function(x) {
      ok <- try(get(x))
      if (!inherits(ok, "try-error") && identical(mode(ok), "function")) {
        cat("\n  ",x," (",is(ok)[1],"): ",sep="")
        
        ##  this function will be used in signalUsageIssue w$warn
        reportFunc <- function(x) {
          cat(x)
          assign("tmpRet", c(tmpRet, x), pos=parent.env(tmpRetEnv))
        }
        codetools::checkUsage(ok, report=reportFunc, all=TRUE)
        
        if (!is.null(tmpRet)) {
          retList[[length(retList) + 1]] <<- tmpRet
          names(retList)[length(retList)] <<- x
        }
        tmpRet <<- NULL
      }
    })
  })
  
  return(invisible(retList))
}


checkCodeFolders <- function(path=".") {
  ##@bdescr
  ##  utility function
  ##  code checks of all .[RrSs] files found in one or more folders
  ##  requires package codetools
  ##@edescr
  ##
  ##@in  path : [character]
  ##@ret      : [list]
  ##
  ##@depends : codetools
  ##
  ##@codestatus : untested
  
  stopifnot(require(codetools))
  if (!is(path, "character")) {
    stop("argument 'path' has to be of type 'character'.")
  }
  if (!all(file.exists(path))) {
    stop("argument 'path' has to contain existing folder(s).")
  }

  
  fNames <- list.files(path=path, pattern="\\.[rRsS]$", full.names=TRUE)

  checkCodeFiles(fNames)
}


checkCodeSweave <- function(path=".") {
  ##@bdescr
  ##  utility function
  ##  code checks of all .[RS]nw files found in one or more folders
  ##  experimental: does not convert extracted code chunks to closures
  ##   thus only functions defined inside a chunk but nut all of the chunk code is checked
  ##
  ##  requires package codetools
  ##@edescr
  ##
  ##@in  path : [character]
  ##@ret      : [list]
  ##@depends : codetools
  ##
  ##@codestatus : untested

  ##  Issue:
  ##  local path e.g. 'RUnit/inst/doc'
  ##  is no expanded to full path
  ##  which I would wnat to use as absolute path
  ##  in the Stangle call
  
  stopifnot(require(utils))
  stopifnot(require(codetools))
  
  if (!is(path, "character")) {
    stop("argument 'path' has to be of type 'character'.")
  }
  if (!all(file.exists(path))) {
    stop("argument 'path' has to contain existing folder(s).")
  }
  browser()
  
  path <- path.expand(path)
  pwd <- getwd()
  if (length(path)) {
    if (path == ".") {
      path <- pwd
    }
    ##  do we have a local path rather then an absolute
    ##  how to infer correct absolute path?
  }
  
  fName <- list.files(path=path, pattern="\\.[RS]nw$", full.names=TRUE)

  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  tmpDir <- file.path(tempdir(), timeStamp)
  print(tmpDir)
  if(!file.exists(tmpDir)) {
    stopifnot(dir.create(tmpDir, recursive=TRUE))
  }
  #on.exit(unlink(tmpDir, recursive=TRUE))

  ##  change to temp folder to dump Stangle output therein
  setwd(tmpDir)
  on.exit(setwd(pwd), add=TRUE)
  
  codeFiles <- unlist(sapply(fName, function(x) {
    Stangle(x)
    x <- basename(x)
    gsub("[RS]nw$", "R", x)
  }))

  ##  
  codeFiles <- file.path(tmpDir, codeFiles)
  checkCodeFiles(codeFiles)
}
