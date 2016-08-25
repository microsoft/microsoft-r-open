# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"), datasets=c("Assay"))

"updateLme.stress" <- function()
{
    
    library(nlme)
    data(Assay)
    as1 <- lme(logDens~sample*dilut, data=Assay,
            random=pdBlocked(list(
                            pdIdent(~1),
                            pdIdent(~sample-1),
                            pdIdent(~dilut-1))))
    
    update(as1,random=pdCompSymm(~sample-1))
    update(as1,random=pdCompSymm(~sample-1))
    update(as1,random=pdCompSymm(~sample-1))
    update(as1,random=pdCompSymm(~sample-1))
}

"test.updateLme.stress" <- function()
{
    res <- try(updateLme.stress())
    checkTrue(!is(res, "try-error"), msg="updateLme stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

