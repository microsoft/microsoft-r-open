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

##  $Id: options.r,v 1.2 2009/11/25 15:03:54 burgerm Exp $

.buildRUnitOptions <- function() {
  ##@bdescr
  ##  Internal function
  ##  adds an entry to R's default global option list
  ##  modelled after version in package Biobase (BioC) 
  ##@edescr
  ##
  ##@ret : [list] extended options() list
  ##
  ##@codestatus : internal
  
  RUnit <- getOption("RUnit")
  if (is.null(RUnit)) {
    RUnit <- list()
    class(RUnit) <- "RUnitOptions"
  }

  if (is.null( RUnit$verbose)) {
    ##  integer: == 0: silent, >= 1: add comments to test case log 
     RUnit$verbose <- 1L
  }
  
  if (is.null(RUnit$silent)) {
    RUnit$silent <- FALSE
  }
  
  if (is.null(RUnit$outfile)) {
    RUnit$outfile <- NULL
  }
  options("RUnit"=RUnit)
}
