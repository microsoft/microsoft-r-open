# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"data.frame.stress" <- function()
{  
    library(nlme)
    subs <- rep(LETTERS[1:10],rep(3,10))
    resp <- rnorm(30)
    ## altered to specify data input since it won't work if called from within a function otherwise
    ##groupedData(resp~1|subs)    
    groupedData(resp~1|subs, data=data.frame(resp=resp, subs=subs))
}

"test.data.frame.stress" <- function()
{
    res <- try(data.frame.stress())
    checkTrue(!is(res, "try-error"), msg="data.frame stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

