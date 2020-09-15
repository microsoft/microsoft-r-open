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
##  $Id: runitPlotConnection.r,v 1.3 2009/04/16 09:18:52 burgerm Exp $


cat("\n\nRUnit test cases for 'RUnit:plotConnection' function\n\n")


testRUnit.plotConnection <- function()
{
  ##@bdescr
  ## test case for function plotConnection of class: none
  ##@edescr

  ##  requires X server to be available for png device
  if (!interactive()) {
    DEACTIVATED("plotConnection uses png device which requires X sverer to be available.")
  }
  
  ##  1) no counts
  conMat <- matrix(0, nrow=5, ncol=5)
  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  tmpPlotFile <- file.path(tempdir(), paste(timeStamp, "connectionPlot.png", sep="_"))
  ret <- RUnit:::plotConnection.trackInfo(conMat, tmpPlotFile)
  checkTrue( is(ret, "NULL"))
  checkTrue( file.exists(tmpPlotFile))
  ##  clean up
  try(unlink(tmpPlotFile))
  
  ##  2)
  num <- 5
  conMat <- matrix(sample(1:3, num^2, replace=TRUE), nrow=num, ncol=num)
  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  tmpPlotFile <- file.path(tempdir(), paste(timeStamp, "connectionPlot2.png", sep="_"))
  ret <- RUnit:::plotConnection.trackInfo(conMat, tmpPlotFile)
  checkTrue( is(ret, "NULL"))
  checkTrue( file.exists(tmpPlotFile))
  ##  clean up
  try(unlink(tmpPlotFile))

  num <- 25
  colNum <- 3
  conMat <- matrix(sample(1:3, num*colNum, replace=TRUE), nrow=num, ncol=colNum)
  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  tmpPlotFile <- file.path(tempdir(), paste(timeStamp, "connectionPlot3.png", sep="_"))
  ret <- RUnit:::plotConnection.trackInfo(conMat, tmpPlotFile)
  checkTrue( is(ret, "NULL"))
  checkTrue( file.exists(tmpPlotFile))
  ##  clean up
  try(unlink(tmpPlotFile))

  num <- 25
  colNum <- 3
  conMat <- matrix(sample(0:3, num*colNum, replace=TRUE), nrow=num, ncol=colNum)
  timeStamp <- format(Sys.time(), "%y%m%d-%H%M")
  tmpPlotFile <- file.path(tempdir(), paste(timeStamp, "connectionPlot4.png", sep="_"))
  ret <- RUnit:::plotConnection.trackInfo(conMat, tmpPlotFile)
  checkTrue( is(ret, "NULL"))
  checkTrue( file.exists(tmpPlotFile))
  ##  clean up
  try(unlink(tmpPlotFile))
}
