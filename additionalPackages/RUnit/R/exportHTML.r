##  RUnit : A unit test framework for the R programming language
##  Copyright (C) 2003-2010  Thomas Koenig, Matthias Burger, Klaus Juenemann
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

##  $Id: exportHTML.r,v 1.12 2010/09/15 13:37:20 burgerm Exp $


plotConnection.trackInfo <- function(con, pngfile, ...)
{
  ##@bdescr
  ##  create a plot displaying the execution flow as a graph
  ##@edescr
  ##
  ##@in  con     : [matrix] counts of execution calls for previous functions
  ##@in  pngfile : [character] string specifying the full path & file name of the
  ##               plot file (PNG) to be generate
  ##@ret         : [NULL] used for its side effect
  ##
  ##@codestatus : testing

  stopifnot(require(graphics))
  ## experimental 2nd order connections
  ## color for arrows
  color <- c("black","lightgreen","green","lightblue","blue","orangered","red")
  ## create nothing if 
  if(all(con==0))
  {
    ## open png device
    grDevices::png(filename=pngfile,width=1024,height=960)
    plot(1:10,axes=FALSE,xlab="",ylab="",main="",type="n")
    text(5,5,labels="No connection graph available")
    grDevices::dev.off()
    return(invisible())
  }
  
  ## overall connections
  allCon <- sum(con)

  ## connections with percent
  con <- ceiling(con/sum(con)*100)

  ## normalize for colors
  con <- (con + 14) %/% 15

  ## open png device
  grDevices::png(filename=pngfile,width=1024,height=960)
  
  ## basic plot
  plot(x=1:nrow(con), y=1:nrow(con), type="n",axes=FALSE,ylab="# line",xlab="",
       ylim=c(nrow(con),1))

  ## draw text lines
  text(x=1, y=1:nrow(con), labels=1:nrow(con))

  ## offset, to avoid complete overlay
  offset <- rep(3,length.out=nrow(con))

  ## minimal x
  xmin <- 2
  
  ## check all connections
  for(i in 1:nrow(con))
  {
    for(j in 1:ncol(con))
    {
      ## check for an existing connection
      if(con[i,j] != 0)
      {
        colDraw <- color[con[i,j]]
        from <- j
        to <- i

        ## circular
        if(from == to)
        {
          top <- from + 0.5
          bot <- from - 0.5
          middle <- (xmin+offset[from])/2

          ## top spline
          splTop <- stats::spline(c(xmin,middle,offset[from]), c(from + 0.2,top,from))

          ## bottom spline
          splBot <- stats::spline(c(xmin,middle,offset[from]), c(from - 0.2,bot,from))
          lines(splTop$x, splTop$y, col=colDraw)
          lines(splBot$x, splBot$y, col=colDraw)
          l <- length(splTop$y)
          
          ## draw arrow tips
          arrows(splTop$x[l-1], splTop$y[l-1], splTop$x[l], splTop$y[l],
                 length=0.04, col=colDraw)
          offset[from] <- offset[from] + 1
          
        } else {
          ## "regular" case
          middle <- (i+j)/2;
          splxy <- stats::spline(c(from - 0.2, middle, to + 0.2),
                          c(xmin - 0.2, offset[from], xmin + 0.2))
          lines(splxy$y, splxy$x, col=colDraw)
          
          if(i < j)
          {
            l <- length(splxy$y)
            ## draw an arrow tip
            arrows(splxy$y[l-1], splxy$x[l-1], splxy$y[l], splxy$x[l],
                   length=0.06, col=colDraw)
          } else {
            ## draw "inverse" arrow tip
            arrows(splxy$y[2], splxy$x[2], splxy$y[1], splxy$x[1],
                   length=0.06, col=colDraw)
          }
          ## set offset higher
          offset[from] <- offset[from] + 1
        }
      }
    }
  }

  legposx <- nrow(con)
  leg.txt <- c("0-15%","15-30%","30-45%","45-60%","60-75%","75-90%","90-100%")
  legend(x=legposx,y=1,legend=leg.txt,lty=1,xjust=1,col=color)
  
  grDevices::dev.off()

  return(invisible())
}


printHTML <- function(object, ...) UseMethod("printHTML")

printHTML.default <- function(object, ...) NextMethod("printHTML")

printHTML.trackInfo <- function(object, baseDir=".")
{
  ##@bdescr
  ##  create a HTML representation of the TrackInfo object data
  ##@edescr
  ##
  ##@in  object : [list] trackInfo object
  ##@in  baseDir   : [character] string specifying the full path to the root directory to hold the HTML pages
  ##
  ##
  ##@codestatus :  untested
   
  ##  preconditions
  if (!is(object, "trackInfo")) {
    stop("argument 'object' has to be a list of class 'trackInfo'.")
  }
  
  if (!is.character(baseDir)) {
    stop("argument 'baseDir' has to be of type 'character'.")
  }
  if (length(baseDir) != 1) {
    stop("argument 'baseDir' has to contain exactly one element.")
  }
  if (is.na(baseDir)) {
    stop("argument 'baseDir' may not be missing value.")
  }
  
  path <- file.path(baseDir,"results")
  if (!file.exists(path))
  {
    ok <- dir.create(path)
    if(!ok) {
      stop(paste("could not create", path) )
    }
  }
  htmlFile <- file.path(path,"index.html")

  footerString <- paste("RUnit ", packageDescription("RUnit", fields="Version"),
                        as.character(Sys.time()))
  
  ## create index.html
  writeHtmlHeader("RUnit Code Inspection - Overview",htmlFile)
  writeHtmlSection("Overview",2,htmlFile)
  writeBeginTable(c("Categ.","Name","Signature"),htmlFile)
  for(i in seq_along(object))
  {
    funcID <- strsplit(names(object)[i],"/")[[1]]
    funcCat <- funcID[1]
    funcName <- funcID[2]
    if(length(funcID) > 2)
    {
      sig <- funcID[3:length(funcID)]
      funcSig <- paste(funcName,"(",paste(sig,collapse=", "),")",sep="")
    } else {
      funcSig <- ""
    }

    writeBeginTag("tr",htmlFile)
    writeCR(htmlFile)
    
    ## write function category
    writeBeginTag("td",htmlFile)
    writeRaw(funcCat,htmlFile)
    writeEndTag("td",htmlFile)
    writeCR(htmlFile)

    ## write function name
    writeBeginTag("td",htmlFile)
    writeLink(file.path(".", paste("result",i,".html",sep="")), funcName, htmlFile)
    writeEndTag("td",htmlFile)
    writeCR(htmlFile)

    ## write function signature
    writeBeginTag("td",htmlFile)
    writeRaw(funcSig,htmlFile)
    writeEndTag("td",htmlFile)
    writeCR(htmlFile)
    writeEndTag("tr",htmlFile)
  }
  writeEndTable(htmlFile)
  writeRaw(footerString, htmlFile)
  writeHtmlEnd(htmlFile)


  writeLinkRef <- function(htmlFile,leftLink,leftName,rightLink,rightName)
  {
    writeBeginTable(c("",""),htmlFile,border=0,width="100%")

    writeBeginTag("tr",htmlFile)
    writeCR(htmlFile)

    writeBeginTag("td",htmlFile)
    writeLink(leftLink,leftName,htmlFile)
    writeEndTag("td",htmlFile)  
    writeCR(htmlFile)

    writeBeginTag("td",htmlFile,"align=\"right\"");
    writeLink(rightLink,rightName,htmlFile)
    writeEndTag("td",htmlFile) 
    writeEndTag("tr",htmlFile) 
    writeCR(htmlFile)

    writeEndTable(htmlFile)
  }
  
  ## create result pages
  for(i in seq_along(object))
  {
    absGraphImg  <- file.path(path, paste("con",i,".png",sep=""))
    absGraphFile <- file.path(path, paste("con",i,".html",sep=""))
    relGraphImg  <- file.path(".", paste("con",i,".png",sep=""))
    relGraphFile <- file.path(".", paste("con",i,".html",sep=""))
    relHTMLFile  <- file.path(".", paste("result",i,".html",sep=""))

    htmlFile <- file.path(path, paste("result",i,".html",sep=""))

    ## begin result page
    writeHtmlHeader("RUnit Code Inspection - Result",htmlFile)


    writeLinkRef(htmlFile,"index.html","index",relGraphFile,"graph")
    
    writeHtmlSep(htmlFile)
    writeHtmlSection("Result",2,htmlFile)

    funcName <- strsplit(names(object)[i],"/")[[1]][2]
    writeRaw("Function: ",htmlFile)
    writeBeginTag("b",htmlFile)
    writeRaw(funcName,htmlFile)
    writeEndTag("b",htmlFile)
    writeCR(htmlFile)

    writeRaw("Runs: ",htmlFile)
    writeBeginTag("b",htmlFile)
    writeRaw(object[[i]]$nrRuns,htmlFile)
    writeEndTag("b",htmlFile)
    writeCR(htmlFile)

    
    writeCR(htmlFile)

    writeBeginTable(c("line","code","calls","time"),htmlFile)
    for(j in seq_along(object[[i]]$src))
    {
      srcLine <- object[[i]]$src[j]
      leadingSpaceNr <- attr(regexpr("^( )*",srcLine),"match.length")
      if(leadingSpaceNr > 0)
      {
         srcLine <- gsub("^( )*","",srcLine)
         srcLine <- paste(paste(rep("&ensp;",leadingSpaceNr),collapse=""),
                          srcLine,collapse="",sep="")
      }
      if(object[[i]]$run[j] > 0)
      {
        bgcolor <- "#00D000"
      } else {
        bgcolor <- "#D00000"
      }
      writeTableRow(c(j,srcLine,object[[i]]$run[j],round(object[[i]]$time[j],2)),
                    htmlFile,bgcolor=bgcolor)
    }

    writeEndTable(htmlFile)
    writeHtmlSep(htmlFile)
    writeLinkRef(htmlFile,"index.html","index",relGraphFile,"graph")
    writeHtmlSep(htmlFile)
    writeRaw(footerString, htmlFile)
    writeHtmlEnd(htmlFile)

    ##  Conncetion plot
    plotConnection.trackInfo(object[[i]]$graph, absGraphImg)
    writeHtmlHeader("RUnit Code Inspection - Connection Graph",absGraphFile)
    writeLinkRef(absGraphFile,"index.html","index",relHTMLFile,"Function")
    writeHtmlSep(absGraphFile)
    writeHtmlSection("Connection Graph",2,absGraphFile)
    writeImage(relGraphImg,absGraphFile)
    writeCR(absGraphFile)
    writeHtmlSep(absGraphFile)
    writeLinkRef(absGraphFile,"index.html","index",relHTMLFile,"Function")
    writeRaw(footerString, absGraphFile)
    writeHtmlEnd(absGraphFile)
    
  }
  
  return(invisible())
}

