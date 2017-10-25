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

##  $Id: inspector.r,v 1.12 2009/04/22 13:50:56 burgerm Exp $


includeTracker <- function(fbody, track=track)
{
  ##@bdescr
  ##  Internal function
  ##
  ##@edescr
  ##
  ##@in  fbody  : [character] vector of code lines of function to track
  ##@in  track  : [trackInfo] list
  ##@ret        : [list] with elements list(modFunc=c(sig,newBody),newSource = newCode)
  ##
  ##@codestatus : internal
  
  ##  get the signature
  sig <- fbody[1]

  ##  get the block structure (important for if, for, while, else with one line)
  block <- sapply(fbody[-1],function(x) regexpr("[^ ]",x)[1], USE.NAMES=FALSE)

  ##  vector of keywords
  kwOpen <- c("for","while","repeat","if","else")

  ##  keyword at begin
  kwGrep <- paste("(",paste(kwOpen,sep="",collapse="|"),")",sep="")


  oneLiner <- function(code)
  {
    ##@bdescr
    ##  utility
    ##  search a character vector ie. the vector of lines of a function body
    ##  for block structures e.g. for|while|repeat|if|else { } code block
    ##@edescr
    ##
    ##@in  code : [character] vector of function body code lines
    ##@ret      : [logical] vector of length of code, indication which are one line control blocks
    ##
    ##@codestatus : internal
    
    return(sapply(code,
                  function(line)
                  {
                    opBr <- grep(paste("^[ ]*",kwGrep,".*[ ]+$",sep=""), line)
                    ##  special case if combined with assignment or math operators
                    opBr2 <- grep(paste("(<-|=|\\+|\\-|\\*|\\/)[ ]*if[ ]*\\(",sep=""), line)
                    if(length(opBr) > 0 || length(opBr2) > 0) {
                      
                      return(TRUE)
                    }
                    return(FALSE)
                  }, USE.NAMES=FALSE))
  }
  
  
  ## set Brackets
  setBrackets <- function(potLine,block,env)
  {
    ##@bdescr
    ##
    ##@edescr
    ##
    ##@in  potLine : [logical] mask vector which line contains a one-line control construct
    ##@in  block   : [integer] vector
    ##@in  env     : [logical] mask vector: which line already contains a opening brace
    ##@ret         : [list] with matching element vectors: openBr & clodeBr
    ##
    ##@codestatus : internal
    
    oBr <- character(length(potLine))
    clBr <- character(length(potLine))
    
    lineIdx <- 1L
    while(lineIdx < length(potLine))
    {
      if(potLine[lineIdx] && !(potLine[lineIdx+1])) {
        oBr[lineIdx] <- "{"
        if (!env[lineIdx+1]) {
          
          clBr[lineIdx+2] <- paste(clBr[lineIdx+2],"}")
          
        } else {

          bbl <- block[lineIdx]
          endBlockIdx <- min(which((bbl >= block) & (seq_along(block) > lineIdx)))
          clBr[endBlockIdx] <- paste(clBr[endBlockIdx],"}")
        }
        
      } else if(potLine[lineIdx] && (potLine[lineIdx+1]) ) {
        oBr[lineIdx] <- "{"
        bbl <- block[lineIdx]
        endBlockIdx <- min(which((bbl >= block) & (seq_along(block) > lineIdx)))
        clBr[endBlockIdx] <- paste(clBr[endBlockIdx],"}")
      }
      
      lineIdx <- lineIdx + 1L
    }
    return(list(openBr=oBr, closeBr=clBr))
  }

  
  ## check for new environments
  env <- sapply(fbody[-1],
                function(code)
                {
                  envIdx <- grep("\\{$",code)
                  if(length(envIdx) > 0)
                  {
                    return(TRUE)
                  }
                  return(FALSE)
                },USE.NAMES=FALSE)
  
  ## check the block structure
  block <- sapply(fbody[-1], function(x) regexpr("[^ ]",x)[1], USE.NAMES=FALSE)

  ## is 4 a convention or a rule?
  block <- (block %/% 4) + 1

  ## check for if's, while's, etc.
  ol <- oneLiner(fbody[-1])

  ## create brackets for control structures without new environments
  br <- setBrackets(ol,block,env)

  ## create new Code
  newCode <- paste(as.vector(rbind(br$closeBr,fbody[-1],br$openBr)))
  newCode <- newCode[newCode != ""]

  ## include the breakpoint function
  bpVec <- sapply(newCode,
                 function(line)
                 {
                   nobp <- grep("^[ ]*(else |\\{|\\})",line)
                   if(length(nobp) == 0)
                   {
                     return("track$bp();")
                   }
                   return("")
                 },USE.NAMES=FALSE)

  for(i in seq_along(bpVec)) {
    bpVec[i] <- gsub("\\(\\)",paste("(",i,")",sep=""),bpVec[i])
  }
  
  ## create the mainpulated body of the function
  newBody <- paste(bpVec,newCode)

  ## return signature and body
  return(list(modFunc=c(sig,newBody), newSource=newCode))
}



tracker <- function()
{
  ##@bdescr
  ##  initialization of the central tracking object
  ##  which stores all information related to inspection results and code execution structure
  ##  defines accessor functions
  ##   - addFunc (fId,src,callExpr): add specified function to the track list
  ##   - getSource(nr): get the source code (character) for function nr on track list
  ##   - init
  ##   - bp
  ##   - getTrackInfo
  ##   - isValidTrackInfo
  ##@edescr
  ##
  ##@ret  : [list] OO object with functions addFunc, getSourcee, init, bp, getTrackInfo
  ##
  ##@codestatus : testing
  
  ## object for information
  trackInfo <- list()
  class(trackInfo) <- "trackInfo"
  
  ## current function index
  fIdx <- 0

  ## old time
  oldTime <- NULL

  ## old src line
  oldSrcLine <- 0
  
  addFunc <- function(fId,src,callExpr)
  {
    ##@bdescr
    ##
    ## accessor function
    ##@edescr
    ##
    ##@in  fId      : [character] function name
    ##@in  src      : [character] source code character vector
    ##@in  callExpr : [character] function call
    ##@ret          : [NULL] returns invisible, used for its side effects
    ##
    ##  codestatus : internal
    
    ##  preconditions
    if( length(fId) != 1) {
      stop("fId must be one character string: function name")
    }
    
    isThere <- which(fId == names(trackInfo))

    if(length(isThere) == 1)
    {
      ##  already in tracking list
      fIdx <<- isThere
    }
    else
    {
      fIdx <<- fIdx + 1
      newFuncInfo <- list(src=src,
                          run=integer(length(src)),
                          time=numeric(length(src)),
                          graph=matrix(0,nrow=length(src),ncol=length(src)),
                          nrRuns=as.integer(0),
                          funcCall=callExpr)

      ##  append strips class attribute
      trackInfo <- append(trackInfo,list(newFuncInfo))
      
      names(trackInfo)[fIdx] <- fId
      class(trackInfo) <- "trackInfo"
      ##  update global state
      trackInfo <<- trackInfo
    }

    ## increment run number
    trackInfo[[fIdx]]$nrRuns <<- trackInfo[[fIdx]]$nrRuns + 1
    
    ## initialize local variables
    oldSrcLine <<- 0
    oldTime <<- NULL

    return(invisible())
  }



  getTrackInfo <- function()
  {
    ##@bdescr
    ##
    ##  accessor function
    ##  returns the main inspection result list with
    ##  elements
    ##   - src
    ##   - run
    ##   - time
    ##   - graph
    ##   - nrRuns
    ##   - funCall
    ##@edescr
    ##
    ##@ret  : [trackInfo] S3 class list (see description above)
    ##
    ##  codestatus : internal
    
    return(trackInfo)
  }
  
  
  init <- function()
  {
    ##@bdescr
    ##
    ##  initalisation function
    ##  sets/resets variables run and fIdx
    ##@edescr
    ##
    ##@ret  : [NULL] returns invisible, used for its side effects
    ##
    ##  codestatus : internal
    
    trackInfoInit <- list()
    class(trackInfoInit) <- "trackInfo"
    trackInfo <<- trackInfoInit
    fIdx <<- 0L
    
    return(invisible())
  }

  
  bp <- function(nr)
  {
    ##@bdescr
    ##
    ##  accessor function
    ##@edescr
    ##
    ##@in   : [integer] index, function run number
    ##@ret  : [NULL] returns invisible, used for its side effects
    ##
    ##  codestatus : internal

    ##  preconditions
    if (length(nr) != 1) {
      stop("argument 'nr' has to be of length 1.")
    }
    if (is.na(nr)) {
      stop("argument 'nr' may not contain missing value (NA).")
    }
    
    trackInfo[[fIdx]]$run[nr] <<- trackInfo[[fIdx]]$run[nr] + 1

    ## cumulative processing time
    if(!is.null(oldTime))
    {
      dtime <-  proc.time()[1] - oldTime
      trackInfo[[fIdx]]$time[nr] <<- trackInfo[[fIdx]]$time[nr] + dtime
    }

    oldTime <<- proc.time()[1]
    ## graph
    if(oldSrcLine != 0)
    { 
      trackInfo[[fIdx]]$graph[oldSrcLine,nr] <<- trackInfo[[fIdx]]$graph[oldSrcLine,nr] + 1
    }

    ## store the old line
    oldSrcLine <<- nr
    
    return(invisible())
  }

  
  getSource <- function(nr)
  {
    ##@bdescr
    ##
    ##  accessor function
    ##  returns the source code as character string
    ##@edescr
    ##
    ##@in   : [integer] index, function run number
    ##@ret  : [character] string, source code
    ##
    ##@codestatus : untested
    
    ##  preconditions
    if (length(nr) != 1) {
      stop("argument 'nr' has to be of length 1.")
    }
    if (is.na(nr)) {
      stop("argument 'nr' may not contain missing value (NA).")
    }
    
    return(trackInfo[[nr]]$src)
  }

  
  isValidTrackInfo <- function(trackInfo) {
    ##@bdescr
    ##  test function
    ##  returns TRUE iff trackInfo object fullfils S3 class definition constraints
    ##    - S3 class 'trackInfo'
    ##    - with elements
    ##      - src      [character] vector of function source code lines
    ##      - run      [integer] vector of no. of times this function was called
    ##      - time     [numeric] vector of function execution times in seconds per call
    ##      - graph    [matrix] connection matrix (# code linbes x # of execution calls)
    ##      - nrRuns   [integer] 
    ##      - funcCall [character] function call
    ##@edescr
    ##
    ##@in   : [trackInfo] S3 class object
    ##@ret  : [logical] TRUE, iff object fullfils class definition constraints
    ##
    ##@codestatus : untested

    if (!is(trackInfo,"trackInfo")) {
      return(FALSE)
    }
    checkElements <- function(x) {
      if (!all(c("src", "run", "time", "graph", "nrRuns", "funcCall") %in% names(x))) {
        return(FALSE)
      }
      if (length(x[["run"]]) < 1 || any(is.na(x[["run"]])) || any(x[["run"]] < 0)) {
        return(FALSE)
      }
      if (length(x[["time"]]) < 1 || any(is.na(x[["time"]])) || any(x[["time"]] < 0)) {
        return(FALSE)
      }

      ##  TODO: graph

      if (length(x[["nrRuns"]]) != 1 || is.na(x[["nrRuns"]]) || x[["nrRuns"]] < 0) {
        return(FALSE)
      }
      if (length(x[["funcCall"]]) != 1 || is.na(x[["funcCall"]])) {
        return(FALSE)
      }
    }
    ok <- sapply(trackInfo, checkElements)
    if (!all(ok)) {
      return(FALSE)
    }
    
    return(TRUE)
  }
  
  return(list(addFunc=addFunc,
              getSource=getSource,
              init=init,
              bp=bp,
              getTrackInfo=getTrackInfo,
              isValid=isValidTrackInfo))
}


inspect <- function(expr, track=track)
{
  ##@bdescr
  ##  inspector function
  ##  an attempt is made to parse the expression or function
  ##  insert track info statements to be used for subsequent
  ##  code execution structure displays
  ##
  ##  can handle functions aswell as generics
  ##@edescr
  ##
  ##@in  expr  : [call]
  ##@in  track : [list] tracker object
  ##@ret       : [expression|ANY] either the unevaluated expression of the function or the result of the function call
  ##
  ##@codestatus : testing
  
  ## get the call and its parameters
  fCall <- as.character(substitute(expr))

  ## get the original call
  callExpr <- deparse(substitute(expr))
  
  ## get the name of the function
  fname <- fCall[1]
  
  ## check for generic function
  if(isGeneric(fname))
  { 
    ## get type of arguments
    selType <- sapply(fCall[-1],
                      function(x)
                      {
                        if(exists(x, envir=sys.parent(sys.parent())))
                        {                        
                          varSig <- is(get(x,envir=sys.parent(sys.parent())))[1]
                        }
                        else
                        {
                          varSig <- is(eval(parse(text=x)))[1]
                        }
                        return(varSig)
                      },USE.NAMES=FALSE)

    
    ## we have to check for missing arguments
    formalArg <- names(formals(getGeneric(fCall[1])))

    ## number of missing arguments
    nrMissing <- length(formalArg) - length(selType)
    if(nrMissing > 0)
    {
      ## check for ...
      ellipseIdx <- which(formalArg == "...")
    
      if(length(ellipseIdx) != 0)
      {
        selType <- c(selType,rep("missing",nrMissing -1 ))
      } else {
        selType <- c(selType,rep("missing",nrMissing))
      }
    }
    ## select function
    selFunc <- selectMethod(fname, selType)

    ## deparse the function
    fbody <- deparse(selFunc@.Data, width.cutoff=500)

    ## create an identifier for the generic function
    fNameId <- paste("S4",fname,paste(selFunc@defined@.Data, collapse="/"), sep="/")
    
  } else {
    ## deparse the function
    fbody <- try(deparse(get(fname), width.cutoff=500), silent=TRUE)
    if (inherits(fbody, "try-error")) {
      ##  in case the function is defined
      ##  in the test case file
      fbody <- try(deparse(get(fname, envir=sys.parent()), width.cutoff=500))
      if (inherits(fbody, "try-error")) {
        stop("function not found.")
      }
    }
    
    ## create an identifier for the generic function
    fNameId <- paste("R/",fname,sep="")
  }  

  
  ## generate the new body of the function
  newFunc <- includeTracker(fbody, track=track)
  track$addFunc(fNameId, newFunc$newSource, callExpr)

  ## build the test function
  eval(parse(text=c("testFunc <- ",newFunc$modFunc)),envir=sys.frame())

  ## create function call
  newFunCall <- paste("testFunc(",paste(fCall[-1], collapse=","), ")",sep="")

  parsedFunc <- try(parse(text=newFunCall))

  ## check for an error
  if(!inherits(parsedFunc,"try-error"))
  {
    ## call the new function
    res <- eval(parsedFunc, envir=parent.frame())
  } else {
    ## no parsing possible
    ## simple call without tracking
    res <- expr
  }


  ## do here some error checking
  
  return(res)
}


