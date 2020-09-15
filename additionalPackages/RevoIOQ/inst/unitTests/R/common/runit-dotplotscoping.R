# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("lattice"))

"dotplotscoping.stress" <- function()
{
    
    postscript("dotplotscoping.ps")
    library(lattice)
    
    fubar <- function() {
        k <- 2
        kkk <- 1:10
        names(kkk) <- 1:10
        data = list(x=kkk)
        dotplot(~x^k + rnorm(10), data)
    }
    
    fubar()
    dev.off()
}

"test.dotplotscoping.stress" <- function()
{
    res <- try(dotplotscoping.stress())
    checkTrue(!is(res, "try-error"), msg="dotplotscoping stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

