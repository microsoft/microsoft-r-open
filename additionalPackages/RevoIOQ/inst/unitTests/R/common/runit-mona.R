# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("cluster","MASS"), datasets=c("animals"))

"mona.stress" <- function()
{
    
    library(cluster)
    
    data(animals)
    (mani <- mona(animals))
    
    str(mani)
    
    if(require(MASS)) {
        
        if(R.version$major != "1" || as.numeric(R.version$minor) >= 7)
            RNGversion("1.6")
        set.seed(253)
        n <- 512; p <- 3
        Sig <- diag(p); Sig[] <- 0.8 ^ abs(col(Sig) - row(Sig))
        x3 <- mvrnorm(n, rep(0,p), Sig) >= 0
        x <- cbind(x3, rbinom(n, size=1, prob = 1/2))
        
        print(sapply(as.data.frame(x), table))
        
        mx <- mona(x)
        str(mx)
        print(lapply(mx[c(1,3,4)], table))
    }
}

"test.mona.stress" <- function()
{
    res <- try(mona.stress())
    checkTrue(!is(res, "try-error"), msg="mona stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

