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

##  $Id: html.r,v 1.10 2009/11/04 17:00:39 burgerm Exp $


writeRaw <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write raw text in a html file
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  cat(htmlStr,file=htmlFile,append=append)
  invisible(TRUE)
}


writeRawCR <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write raw text in a html file with a cr at end
  ##@bdescr
  ##@in htmlStr  : [character] text
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal 

  writeRaw(htmlStr,htmlFile,append)
  cat("\n",file=htmlFile,append=TRUE)
  invisible(TRUE)
}


writeTitle <- function(htmlStr,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write title tags and title text
  ##@bdescr
  ##@in htmlStr  : [character] title
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
   
  writeRaw("<title>",htmlFile,append)
  writeRaw(htmlStr,htmlFile)
  writeRaw("</title>\n",htmlFile)
}


writeBeginHead <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write <head>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("<head>",htmlFile,append)
}


writeEndHead <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write </head>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("</head>\n",htmlFile,append)
}


writeBeginHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write <html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("<html>",htmlFile,append)
}


writeEndHtml <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write </html>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("</html>\n",htmlFile,append)
}


writeBeginBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write <body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("<body>",htmlFile,append)
}


writeEndBody <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write </body>
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw("</body>\n",htmlFile,append)
}


writeBeginTag <- function(htmlTag,htmlFile,para="",append=TRUE)
{
  ##@bdescr
  ## private function
  ## write begin of a tag, with parameters
  ##@bdescr
  ##@in htmlTag  : [character] name of the tag
  ##@in htmlFile : [character] name of the html file
  ##@in para     : [character] parameters as string
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
   
  if(all(para =="")) {
    writeRaw(paste("<",htmlTag,">",sep=""),htmlFile,append)
  } else {
    writeRaw(paste("<",htmlTag," ",para,">",sep=""),htmlFile,append)
  }

}


writeEndTag <- function(htmlTag,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write end of tag
  ##@bdescr
  ##@in htmlTag  : [character] name of the tag
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeRaw(paste("</",htmlTag,">",sep=""),htmlFile,append)
}


writeCR <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write CR in html file for better formatting of the html source
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  cat("\n",file=htmlFile,append=append)
  invisible(TRUE)
}


writeBeginTable <- function(header,htmlFile,border=1,
                            width="100%",append=TRUE,
                            columnWidth=NULL)
{
  ##@bdescr
  ## private function
  ## write begin of a table
  ##@bdescr
  ##@in header   : [character] title for columns
  ##@in htmlFile : [character] name of the html file
  ##@in border   : [integer] border of table
  ##@in width    : [character] width of table
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  tablePara <- paste("border=\"",border,"\" width=\"",width,"\"",sep="")
  writeRawCR(paste("<table ",tablePara," >",sep=""),htmlFile,append)

  ##  if header is provided
  if (length(header) > 0) {
    writeBeginTag("tr",htmlFile)

    
    for(i in seq_along(header)) {
      para <- ""
      if(!is.null(columnWidth)) {
        if (length(columnWidth) == length(header)) {
          para = paste("width=\"", columnWidth[i], "\"", sep="")
        } else {
          ##  recycle first
          para = paste("width=\"", columnWidth[1], "\"", sep="")
        }
      }
      writeBeginTag("th",htmlFile, para=para)
      writeRaw(header[i],htmlFile)
      writeEndTag("th",htmlFile)
      writeCR(htmlFile)
    }
    
    writeEndTag("tr",htmlFile,append)
  }
  
  writeCR(htmlFile)
}


writeTableRow <- function(row,htmlFile,append=TRUE,bgcolor="")
{
  ##@bdescr
  ## private function
  ## write a table row
  ##@bdescr
  ##@in row      : [character] data for table cells in row
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@in bgcolor  : [character] color for table cells
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeBeginTag("tr",htmlFile)
  if(length(bgcolor) == 1)
  {
    bgcolor <- rep(bgcolor,length(row))
  }
  for(i in seq_along(row))
  {
    if(bgcolor[i] == "")
    {
      writeBeginTag("td",htmlFile)
    } else {
      writeBeginTag("td",htmlFile,para=paste("bgcolor=\"",bgcolor[i],"\"",sep=""))
    }
    writeRaw(row[i],htmlFile)
    writeEndTag("td",htmlFile)
    writeCR(htmlFile)
  }

  writeEndTag("tr",htmlFile,append)
  writeCR(htmlFile)
}


writeLink <- function(target,name,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write a link
  ##@bdescr
  ##@in target   : [character] target of the link
  ##@in name     : [character] name of the target
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
    
  writeBeginTag("a",htmlFile,paste("href=\"",target,"\"",sep=""),append=append)
  writeRaw(name,htmlFile,append=TRUE)
  writeEndTag("a",htmlFile,append=TRUE)
}


writeEndTable <- function(htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## close an <table> enviroment by adding </table>
  #@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  writeEndTag("table",htmlFile,append)
  writeCR(htmlFile)
}


writeHtmlHeader <- function(header,htmlFile)
{
  ##@bdescr
  ## private function
  ## write a HTML file header
  ##  - DOCTYPE
  ##  - <html>
  ##  - <head> <title> </head>
  ##  - <body>
  ##
  ## should be finished by writeHtmlEnd 
  ##@bdescr
  ##@in header   : [character] title of the document
  ##@in htmlFile : [character] name of the link
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  writeRawCR("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\"",
             htmlFile,FALSE)
  writeRawCR("\"http://www.w3.org/TR/html4/transitional.dtd\">",htmlFile)
  writeBeginHtml(htmlFile)
  writeBeginHead(htmlFile)
  writeTitle(header,htmlFile)
  writeEndHead(htmlFile)
  writeBeginBody(htmlFile)
}


writeHtmlEnd <- function(htmlFile)
{
  ##@bdescr
  ## private function
  ## write end of html code
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  writeEndBody(htmlFile)
  writeEndHtml(htmlFile)
}


writeHtmlSep <- function(htmlFile)
{
  ##@bdescr
  ## private function
  ## write horizontal seperator
  ##@bdescr
  ##@in htmlFile : [character] name of the html file
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  writeRawCR("<hr>",htmlFile)
}


writeImage <- function(img,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write image tags
  ##@bdescr
  ##@in img :      [character] name of the image file
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  writeBeginTag("img",htmlFile,para=paste("src=\"",img,"\"",sep=""),append)
  writeEndTag("img",htmlFile)
}


writeHtmlSection <- function(title,sec,htmlFile,append=TRUE)
{
  ##@bdescr
  ## private function
  ## write titles for section
  ##@bdescr
  ##@in title    : [character] title of the section
  ##@in sec      : [integer] size of title (between 1-6)
  ##@in htmlFile : [character] name of the html file
  ##@in append   : [logical] append the html code
  ##@ret         : [logical] TRUE if execution completes
  ##
  ##@codestatus : internal
  
  secTag <- paste("h",sec,sep="")
  writeBeginTag(secTag,htmlFile,append)
  writeRaw(title,htmlFile,append)
  writeEndTag(secTag,htmlFile,append)
  writeCR(htmlFile,append)
}


writeHtmlTable <- function(dataFrame, htmlFile, border=1,
                           width="100%", append=TRUE)
{
  ##@bdescr
  ## private function
  ## write a data frame to a HTML table 
  ##@bdescr
  ##
  ##@in dataFrame : [data frame] size of title (between 1-6)
  ##@in htmlFile  : [character] name of the html file
  ##@in border    : [integer] 1 (default) table borders will be shown
  ##@in width     : [character] width of table
  ##@in append    : [logical] if TRUE append the tabel to an existing HTML file
  ##@ret          : [logical] TRUE if execution completed
  ##
  ##@codestatus   : internal

  header <- NULL
  colNames <- colnames(dataFrame)
  if (!is.null(colNames)) {
    if (length(colNames) == dim(dataFrame)[2]) {
      header <- colNames
    } else {
      ##  don't write column names
      header <- NULL
    }
  }

  rowNames <- rownames(dataFrame)
  if (!is.null(rowNames)) {
    header <- c("Name", header)
    dataFrame <- cbind(rowNames, dataFrame)
  }
  writeBeginTable(header, htmlFile, border=border,
                  width=width, append=append,
                  columnWidth=NULL)
  
  for (ti in 1:dim(dataFrame)[1]) {
    writeTableRow(dataFrame[ti, ], htmlFile, append=TRUE, bgcolor="")
  }
  writeEndTable(htmlFile,append=TRUE)
}
