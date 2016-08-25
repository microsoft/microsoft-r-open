# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("survival","rpart","splines"))

"surv.test.stress" <- function()
{
    
    ## example from coxph: gave error in rpart 3.1-31
    ## due to missing drop = FALSE
    test1 <- list(time=  c(4, 3,1,1,2,2,3),
            status=c(1,NA,1,0,1,1,0),
            x=     c(0, 2,1,1,1,0,0),
            sex=   c(0, 0,0,0,1,1,1))
    library(survival)
    library(rpart)
    rpart(Surv(time, status) ~ x + sex, test1)
}

"test.surv.test.stress" <- function()
{
    res <- try(surv.test.stress())
    checkTrue(!is(res, "try-error"), msg="surv.test stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

