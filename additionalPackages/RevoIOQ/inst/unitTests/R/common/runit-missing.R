# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"missing.stress" <- function()
{
    
    library(nlme)
    
    options(digits=4) # avoid rounding differences
    
    Ovary[c(1,272), 2] <- NA
    fm1 <- gls(follicles ~ sin(2*pi*Time) + cos(2*pi*Time), Ovary,
            correlation = corAR1(form = ~ 1 | Mare), na.action=na.exclude)
    fitted(fm1)
    residuals(fm1)
    summary(fm1)
    
    Orthodont[100:102, 2] <- NA
    fm2 <- lme(distance ~ age + Sex, data = Orthodont, random = ~ 1,
            na.action=na.exclude)
    fitted(fm2, 0:1)
    fitted(fm2)
    residuals(fm2, 0:1)
    round(residuals(fm2), 2)
    summary(fm2)
    
    Soybean[1:5, "Time"] <- NA
    fm3 <- gnls(weight ~ SSlogis(Time, Asym, xmid, scal), Soybean,
            weights = varPower(), na.action=na.exclude)
    fitted(fm3)
    residuals(fm3)
    summary(fm3)
}

"test.missing.stress" <- function()
{
    res <- try(missing.stress())
    checkTrue(!is(res, "try-error"), msg="missing stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

