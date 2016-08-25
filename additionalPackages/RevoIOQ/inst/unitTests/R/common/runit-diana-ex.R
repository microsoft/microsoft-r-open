# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("cluster"), datasets=c("votes.repub","agriculture","ruspini","swiss"))

"diana.ex.stress" <- function()
{
    
    library(cluster)
    options(digits = 6)
    data(votes.repub)
    
    .proctime00 <- proc.time()
    
    summary(diana(votes.repub, metric = "manhattan", stand = TRUE))
    summary(diana(daisy(votes.repub), keep.diss = FALSE))
    
    data(agriculture)
    summary(diana(agriculture))
    
    data(ruspini)
    summary(dr0 <- diana(ruspini, keep.diss=FALSE, keep.data=FALSE))
    summary(dr1 <- diana(ruspini, metric = "manhattan"))
    str(dr1)
    
    cat('Time elapsed: ', proc.time() - .proctime00,'\n')
    
    ## From system.file("scripts/ch11.R", package = "MASS")
    data(swiss)
    swiss.x <- as.matrix(swiss[,-1])
    dCH <- diana(swiss.x)
    str(as.dendrogram(as.hclust(dCH)))
    
}

"test.diana.ex.stress" <- function()
{
    res <- try(diana.ex.stress())
    checkTrue(!is(res, "try-error"), msg="diana.ex stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

