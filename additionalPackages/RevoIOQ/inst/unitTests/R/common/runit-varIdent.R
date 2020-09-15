# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"varIdent.stress" <- function()
{
    
    ## test for PR#9765
    library(nlme)
    Orth <- subset(Orthodont, Subject %in% c("M01","F01"))
# Argument fixed in varIdent is ignored
    vf  <- varIdent(form=~1|Sex,fixed=c(Female=0.5))
    vf <- Initialize(vf, data=Orth)
    stopifnot(varWeights(vf) == rep(c(1,2), each=4))
}

"test.varIdent.stress" <- function()
{
    res <- try(varIdent.stress())
    checkTrue(!is(res, "try-error"), msg="varIdent stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

