# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"), datasets=c("Orthodont"))

"augPredmissing.stress" <- function()
{
    
    library(nlme)
    data(Orthodont)
# add a column with an NA that is not used in the fit
    Orthodont$Others = runif(nrow(Orthodont))
    is.na(Orthodont$Others[3]) = TRUE
    fm1 = lme(Orthodont, random = ~1)
    augPred(fm1, length.out = 2, level = c(0,1))
}

"test.augPredmissing.stress" <- function()
{
    res <- try(augPredmissing.stress())
    checkTrue(!is(res, "try-error"), msg="augPredmissing stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

