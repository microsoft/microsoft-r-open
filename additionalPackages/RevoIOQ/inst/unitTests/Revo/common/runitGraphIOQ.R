# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages="tools")

graphicsDevice.stress.png <- function()
{
    ##
    ## Testing png graphics device
    ##
    png(filename = "testplot.png")
    plot(1:10)
    dev.off()
    checkTrue(file.exists("testplot.png"))
    unlink("testplot.png")
}

graphicsDevice.stress.jpg <- function()
{
    ##
    ## Testing jpg graphics device
    ##
    jpeg(filename = "testplot.jpg")
    plot(1:10)
    dev.off()
    checkTrue(file.exists("testplot.jpg"))
    unlink("testplot.jpg")
}

## CAIRO graphics device tests
graphicsDevice.stress.svg <- function()
{
    ##
    ## Testing svg graphics device
    ##
    svg(filename = "testplot.svg")
    plot(1:10)
    dev.off()
    checkTrue(file.exists("testplot.svg"))
    unlink("testplot.svg")
}

graphicsDevice.stress.cairo_pdf <- function()
{
    ##
    ## Testing Cairo PDF graphics device
    ##
    cairo_pdf(filename = "testplot.pdf")
    plot(1:10)
    dev.off()
    checkTrue(file.exists("testplot.pdf"))
    unlink("testplot.pdf")
}

graphicsDevice.stress.cairo_ps <- function()
{
    ##
    ## Testing Cairo PS graphics device
    ##
    cairo_ps(filename = "testplot.ps")
    plot(1:10)
    dev.off()
    checkTrue(file.exists("testplot.ps"))
    unlink("testplot.ps")
}
"test.graphicsDevices.stress" <- function()
{
    res <- try(graphicsDevice.stress.png())
    checkTrue(!is(res, "try-error"), msg="PNG Graphics Device stress test failed")    
    res <- try(graphicsDevice.stress.jpg())
    checkTrue(!is(res, "try-error"), msg="JPG Graphics Device stress test failed")
    res <- try(graphicsDevice.stress.svg())
    checkTrue(!is(res, "try-error"), msg="SVG Graphics Device stress test failed")  
    res <- try(graphicsDevice.stress.cairo_pdf())
    checkTrue(!is(res, "try-error"), msg="Cairo PDF Graphics Device stress test failed")  
    res <- try(graphicsDevice.stress.cairo_ps())
    checkTrue(!is(res, "try-error"), msg="Cairo PS Graphics Device stress test failed")  	
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}


