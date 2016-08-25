# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("cluster"))

"ellipsoid.ex.stress" <- function()
{
    
    library(cluster)
    
    eh <- ellipsoidhull(cbind(x=1:4, y = 1:4)) #singular
    eh
    
    set.seed(157)
    for(n in 4:10) { ## n=2 and 3 still differ -- platform dependently!
        cat("n = ",n,"\n")
        x2 <- rnorm(n)
        print(ellipsoidhull(cbind(1:n, x2)))
        print(ellipsoidhull(cbind(1:n, x2, 4*x2 + rnorm(n))))
    }
    
    x <- rt(100, df = 4)
    y <- 100 + 5 * x + rnorm(100)
    ellipsoidhull(cbind(x,y))
    z <- 10  - 8 * x + y + rnorm(100)
    ellipsoidhull(cbind(x,y,z))
}

"test.ellipsoid.ex.stress" <- function()
{
    res <- try(ellipsoid.ex.stress())
    checkTrue(!is(res, "try-error"), msg="ellipsoid.ex stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

