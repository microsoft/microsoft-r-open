# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"coef.stress" <- function()
{
    
    ## tests of fix for PR#9831
    library(nlme)
    val <- c("10"=1.10,"14"=1.14)
    vf <- varIdent(value=val, form=~1|age, fixed=c("12"=1.12))
    vfi <- Initialize(vf,Orthodont)
    str(vfi)
    coef(vfi)
    coef(vfi, unconstrained = FALSE, allCoef = TRUE)
    vfiCopy <- vfi        # copy of an initialized object
    length(vfiCopy)             # length is 2
    coef(vfiCopy) <- c(11,12)   # error in 3.1-84
    
    ## error in 3.1-84
    gls.error  <- gls(distance ~ age, weights = vfi, data=Orthodont)
}

"test.coef.stress" <- function()
{
    res <- try(coef.stress())
    checkTrue(!is(res, "try-error"), msg="coef stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

