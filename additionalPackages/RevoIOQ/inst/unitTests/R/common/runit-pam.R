# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages=c("cluster"), datasets=c("ruspini","votes.repub"))

"pam.stress" <- function()
{
    
    library(cluster)
    ## Compare on these:
    nms <- c("clustering", "objective", "isolation", "clusinfo", "silinfo")
    nm2 <- c("medoids", "id.med", nms)
    nm3 <- nm2[- pmatch("obj", nm2)]
    
    (x <- x0 <- cbind(V1 = (-3:4)^2, V2 = c(0:6,NA), V3 = c(1,2,NA,7,NA,8:9,8)))
    (px <- pam(x,2, metric="manhattan"))
    stopifnot(identical(x,x0))# DUP=FALSE ..
    pd <-  pam(dist(x,"manhattan"), 2)
    px2 <- pam(x,2, metric="manhattan", keep.diss=FALSE, keep.data=FALSE)
    pdC <- pam(x,2, metric="manhattan", cluster.only = TRUE)
    
    stopifnot(identical(px[nms], pd[nms]),
            identical(px[nms], px2[nms]),
            identical(pdC, px2$clustering),
            ## and for default dist "euclidean":
            identical(pam(x,	2)[nms],
                    pam(dist(x),2)[nms])
    )
    
    set.seed(253)
    ## generate 250 objects, divided into 2 clusters.
    x <- rbind(cbind(rnorm(120, 0,8), rnorm(120, 0,8)),
            cbind(rnorm(130,50,8), rnorm(130,10,8)))
    
    .proctime00 <- proc.time()
    
    summary(px2 <- pam(x, 2))
    pdx <- pam(dist(x), 2)
    all.equal(px2[nms], pdx[nms], tol = 1e-12) ## TRUE
    pdxK <- pam(dist(x), 2, keep.diss = TRUE)
    stopifnot(identical(pdx[nm2], pdxK[nm2]))
    
    spdx <- silhouette(pdx)
    summary(spdx)
    spdx
    postscript("pam-tst.ps")
    if(FALSE)
        plot(spdx)# the silhouette
    ## is now identical :
    plot(pdx)# failed in 1.7.0 -- now only does silhouette
    
    par(mfrow = 2:1)
    ## new `dist' argument for clusplot():
    plot(pdx, dist=dist(x))
    ## but this should work automagically (via eval()) as well:
    plot(pdx)
    ## or this
    clusplot(pdx)
    
    data(ruspini)
    summary(pr4 <- pam(ruspini, 4))
    (pr3 <- pam(ruspini, 3))
    (pr5 <- pam(ruspini, 5))
    
    data(votes.repub)
    summary(pv3 <- pam(votes.repub, 3))
    (pv4 <- pam(votes.repub, 4))
    (pv6 <- pam(votes.repub, 6, trace = 3))
    
    cat('Time elapsed: ', proc.time() - .proctime00,'\n')
    
    ## re-starting with medoids from pv6  shouldn't change:
    pv6. <- pam(votes.repub, 6, medoids = pv6$id.med, trace = 3)
    identical(pv6[nm3], pv6.[nm3])
    
    ## This example seg.faulted at some point:
    d.st <- data.frame(V1= c(9, 12, 12, 15, 9, 9, 13, 11, 15, 10, 13, 13,
                    13, 15, 8, 13, 13, 10, 7, 9, 6, 11, 3),
            V2= c(5, 9, 3, 5, 1, 1, 2, NA, 10, 1, 4, 7,
                    4, NA, NA, 5, 2, 4, 3, 3, 6, 1, 1),
            V3 = c(63, 41, 59, 50, 290, 226, 60, 36, 32, 121, 70, 51,
                    79, 32, 42, 39, 76, 60, 56, 88, 57, 309, 254),
            V4 = c(146, 43, 78, 88, 314, 149, 78, NA, 238, 153, 159, 222,
                    203, NA, NA, 74, 100, 111, 9, 180, 50, 256, 107))
    dd <- daisy(d.st, stand = TRUE)
    (r0 <- pam(dd, 5))# cluster 5 = { 23 } -- on single observation
    ## This gave only 3 different medoids -> and seg.fault:
    (r5 <- pam(dd, 5, medoids = c(1,3,20,2,5), trace = 2)) # now "fine"
    
    dev.off()
    
    ## Last Line:
    cat('Time elapsed: ', proc.time() - .proctime00,'\n')
    
}

"test.pam.stress" <- function()
{
    res <- try(pam.stress())
    checkTrue(!is(res, "try-error"), msg="pam stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}

