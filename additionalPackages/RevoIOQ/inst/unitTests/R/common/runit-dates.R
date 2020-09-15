# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("lattice"))

"dates.stress" <- function()
{
    
    
    postscript("date.ps")
    library(lattice)
    
    ## POSIXt handling
    
    y <- Sys.time() + 10000 * 1:100
    x <- rnorm(100)
    b <- gl(3,1,100)
    
    xyplot(y ~ x | b)
    xyplot(y ~ x | b, scales = list(relation = "free", rot = 0))
    xyplot(y ~ x | b, scales = "sliced")
    
    ## Date handling
    
    dat <-
            data.frame(a = 1:10,
                    b = seq(as.Date("2003/1/1"), as.Date("2003/1/10"), by="day"))
    xyplot(a~b, dat)
    xyplot(a~b, dat, scales=list(x=list(at=dat$b)))
    
    
    dev.off()
    
}

"test.dates.stress" <- function()
{
    res <- try(dates.stress())
    checkTrue(!is(res, "try-error"), msg="dates stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

