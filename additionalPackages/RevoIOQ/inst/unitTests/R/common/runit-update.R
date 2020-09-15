# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"), datasets=c("petrol"))

"update.stress" <- function()
{
    library(nlme)
    data(petrol, package = 'MASS')
    Petrol <- petrol
    Petrol[, 2:5] <- scale(Petrol[, 2:5], scale = FALSE)
    pet3.lme <- lme(Y ~ SG + VP + V10 + EP,
            random = ~ 1 | No, data = Petrol, method="ML")
    nform <- as.formula("Y ~ SG + VP + V10")
    update(pet3.lme, nform)
}

"test.update.stress" <- function()
{
    res <- try(update.stress())
    checkTrue(!is(res, "try-error"), msg="update stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

