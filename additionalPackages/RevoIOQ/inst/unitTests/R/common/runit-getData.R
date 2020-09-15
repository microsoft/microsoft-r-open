# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"getData.stress" <- function()
{
    
    library(nlme)
    fm1 <- lme(distance ~ age, Orthodont)
    str(o1 <- getData(fm1))
    
    df <- Orthodont # note that the name conflicts with df in the stats
    fm2 <- lme(distance ~ age, df)
    str(o2 <- getData(fm2))
    stopifnot(identical(o1, o2))
}

"test.getData.stress" <- function()
{
    res <- try(getData.stress())
    checkTrue(!is(res, "try-error"), msg="getData stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

