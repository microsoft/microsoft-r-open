# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("graphics"))

"reg.plot.latin1.stress" <- function()
{
    
    postscript(file = "reg-plot-latin1.ps",
            encoding = "ISOLatin1",
            width = 7, height = 7)
    library(graphics) # to be sure
    example(text)     # has Latin-1 examples
    #q("no")
}

"test.reg.plot.latin1.stress" <- function()
{
    DEACTIVATED("This test currently fails in shipR")
    res <- try(reg.plot.latin1.stress())
    checkTrue(!is(res, "try-error"), msg="reg.plot.latin1 stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

