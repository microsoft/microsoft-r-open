# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession()

"sas.stress" <- function()
{
    
    if(!nzchar(Sys.getenv("R_FOREIGN_FULL_TEST")))
    {
        warning("!nzchar(Sys.getenv(\"R_FOREIGN_FULL_TEST\"))") 
        return()
#q("no")
    }
    
    library("foreign")
    setwd(tempdir())
    tfile <- "int1982ag.zip"
    download.file("ftp://cusk.nmfs.noaa.gov/mrfss/intercept/ag/int1982ag.zip",
            tfile, quiet=TRUE, mode="wb")
    zip.file.extract("int1982ag.xpt", tfile)
    dfs <- read.xport("int1982ag.xpt")
    foo <- dfs$I3_19822
    nrow(foo)
    stopifnot(nrow(foo) == 3650)
}

"test.sas.stress" <- function()
{
    res <- try(sas.stress())
    checkTrue(!is(res, "try-error"), msg="sas stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

