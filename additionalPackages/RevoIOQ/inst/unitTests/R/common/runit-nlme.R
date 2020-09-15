# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"), datasets=c("Loblolly"))

"nlme.stress" <- function()
{
    
    library(nlme)
    data(Loblolly)
    fm1 <- nlsList(SSasymp, Loblolly)
    fm1
    fm2 <- nlme(fm1, random = Asym ~ 1)
    fm2
#q()
}

"test.nlme.stress" <- function()
{
    res <- try(nlme.stress())
    checkTrue(!is(res, "try-error"), msg="nlme stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

