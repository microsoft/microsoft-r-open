# file RODBC/R/win.R
# copyright (C) 2002-2009  B. D. Ripley
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 or 3 of the License
#  (at your option).
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#

if(.Platform$OS.type == "windows") {

    ## FIXME: use grepl for is.abs in due course (R >= 2.9.0)
    full.path <- function(filename) {
        fn <- gsub("\\", "/", filename, fixed = TRUE)
        is.abs <- length(grep("^[ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz]:|/", fn)) > 0L
        gsub("/", "\\",
             if(!is.abs) file.path(getwd(), filename) else filename,
             fixed = TRUE)
    }

    ## originally based on suggestions from xiao.gang.fan1@libertysurf.fr
    odbcConnectExcel <- function(xls.file, readOnly = TRUE, ...)
    {
        if (.Machine$sizeof.pointer > 4)
            stop("odbcConnectExcel is only usable with 32-bit Windows")
        con <- if(missing(xls.file))
            "Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq="
        else {
            fp <- full.path(xls.file)
            paste("Driver={Microsoft Excel Driver (*.xls)};DriverId=790;Dbq=",
                  fp, ";DefaultDir=", dirname(fp), ";", sep = "")
        }
	if(!readOnly) con = paste(con, "ReadOnly=False", sep=";")
        odbcDriverConnect(con, tabQuote=c("[", "]"), ...)
    }

    odbcConnectExcel2007 <- function(xls.file, readOnly = TRUE, ...)
    {
        con <- if(missing(xls.file))
            "Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq="
        else {
            fp <- full.path(xls.file)
            paste("Driver={Microsoft Excel Driver (*.xls, *.xlsx, *.xlsm, *.xlsb)};Dbq=",
                  fp, ";DefaultDir=", dirname(fp), ";", sep = "")
        }
	if(!readOnly) con = paste(con, "ReadOnly=False", sep=";")
        odbcDriverConnect(con, tabQuote=c("[", "]"), ...)
    }

    odbcConnectAccess <- function(access.file, uid = "", pwd = "", ...)
    {
        if (.Machine$sizeof.pointer > 4)
            stop("odbcConnectAccess is only usable with 32-bit Windows")
        con <- if(missing(access.file))
            "Driver={Microsoft Access Driver (*.mdb)};Dbq="
        else
            paste("Driver={Microsoft Access Driver (*.mdb)};Dbq=",
                  full.path(access.file),
                  ";Uid=", uid, ";Pwd=", pwd, ";", sep="")
        odbcDriverConnect(con, ...)
    }

    odbcConnectAccess2007 <- function(access.file, uid = "", pwd = "", ...)
    {
        con <- if(missing(access.file))
            "Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq="
        else
            paste("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                  full.path(access.file),
                  ";Uid=", uid, ";Pwd=", pwd, ";", sep="")
        odbcDriverConnect(con, ...)
    }

    odbcConnectDbase <- function(dbf.file, ...)
    {
        if (.Machine$sizeof.pointer > 4)
            warning("odbcConnectDbase is probably only usable with 32-bit Windows")
        con <- if(missing(dbf.file))
            "Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq="
        else
            paste("Driver={Microsoft dBASE Driver (*.dbf)};DriverID=277;Dbq=",
                     dirname(full.path(dbf.file)), ";", sep="")
        odbcDriverConnect(con, tabQuote=c("[", "]"), ...)
    }
}
