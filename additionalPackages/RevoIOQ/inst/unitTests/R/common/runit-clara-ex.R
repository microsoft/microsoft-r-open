# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("cluster"), datasets=c("xclara"))

"clara.ex.stress" <- function()
{
    
    #### These are *NOT* compared with output in the released version of
    ###  `cluster'  currently
    
    library(cluster)
    
    data(xclara)
    ## Try 100 times *different* random samples -- for reliability:
    if(R.version$major != "1" || as.numeric(R.version$minor) >= 7) RNGversion("1.6")
    
    nSim <- 100
    nCl <- 3 # = no.classes
    set.seed(421)# (reproducibility)
    ## unknown problem: this is still platform dependent to some extent:
    cl <- matrix(NA,nrow(xclara), nSim)
    for(i in 1:nSim) cl[,i] <- clara(xclara, nCl, rngR = TRUE)$cluster
    tcl <- apply(cl,1, tabulate, nbins = nCl)
    ## those that are not always in same cluster (5 out of 3000 for this seed):
    (iDoubt <- which(apply(tcl,2, function(n) all(n < nSim))))
    if(length(iDoubt)) { # (not for all seeds)
        tabD <- tcl[,iDoubt, drop=FALSE]
        dimnames(tabD) <- list(cluster = paste(1:nCl), obs = format(iDoubt))
        t(tabD) # how many times in which clusters
    }
}

"test.clara.ex.stress" <- function()
{
    res <- try(clara.ex.stress())
    checkTrue(!is(res, "try-error"), msg="clara.ex stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

