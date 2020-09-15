# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("nlme"))

"gls.stress" <- function()
{
    
    ## reported by simon bond <shug0131@yahoo.co.uk> to R-help 2007-03-16
    
    library(nlme)
    x <- rnorm(10, 0.1, 1)
    try(gls(x ~ 0))  # segfaulted in 3.1-79
    
    
    ## PR#10364
# copied verbatim from Pinheiro & Bates 8.3.3
    fm1Dial.gnls <-
            gnls(rate ~ SSasympOff(pressure, Asym, lrc, c0),
                    data = Dialyzer, params = list(Asym + lrc ~ QB, c0 ~ 1),
                    start = c(53.6, 8.6, 0.51, -0.26, 0.225))
    (p1 <- predict(fm1Dial.gnls))
    (p2 <- predict(fm1Dial.gnls, newdata = Dialyzer))
# failed, factor levels complaint
# also, missed row names as names
    stopifnot(all.equal(as.vector(p1), as.vector(p2)), # 'label' differs
            identical(names(p1), names(p2)))
}

"test.gls.stress" <- function()
{
    res <- try(gls.stress())
    checkTrue(!is(res, "try-error"), msg="gls stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

