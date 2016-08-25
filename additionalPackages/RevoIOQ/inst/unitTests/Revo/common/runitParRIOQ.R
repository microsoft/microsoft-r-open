# Copyright (c) 2015 Microsoft Corporation All Rights Reserved

session <- RevoIOQ:::saveRUnitSession(packages="tools")

test.foreach <- function(){
	##
	## testing function foreach
	##
    if (identical(system.file(package="RevoScaleR"), ""))
    {
        DEACTIVATED("Test deactivated because Revolution Enterprise components are not installed on this system.")
    }
	library(foreach)
	x <- matrix(rnorm(100*100), 100)
	cmeans <- foreach(i=1:ncol(x), .combine=c) %do% mean(x[,i])
	expected <- colMeans(x)
	checkTrue(all.equal(cmeans, expected), msg="foreach test failed")

	library(doParallel)
	##NOTE: stopping a child process spawned by forking also stops RevoFIFO and thus BxlServer
	##	which however are still needed by the parent process that spawned the child using fork.
	#registerDoParallel(2)
	#on.exit(stopImplicitCluster())
	cl <- makePSOCKcluster(2)
	registerDoParallel(cl)
	on.exit(stopCluster(cl))
	cmeans <- foreach(i=1:ncol(x), .combine=c) %dopar% mean(x[,i])
	checkTrue(all.equal(cmeans, expected), msg="foreach test failed")

	x <- matrix(rnorm(16), 4)
	y <- matrix(rnorm(16), 4)
	z <- foreach(y=iter(y, by='col'), .combine=cbind) %dopar% (x %*% y)
	checkTrue(all.equal(z, x %*% y), msg="foreach test failed")
}


stress.iterators <- function()
{
	library(iterators)
	it1 <- iter(c('a', 'b', 'c'))
	nextElem(it1)
	nextElem(it1)
	nextElem(it1)

	it2 <- iter(function() runif(1))
	nextElem(it2)
	nextElem(it2)
	nextElem(it2)
}


"test.iterators.stress" <- function()
{
    if (identical(system.file(package="foreach"), ""))
    {
        DEACTIVATED("Test deactivated because Revolution Enterprise components are not installed on this system.")
    }
    res <- try(stress.iterators())
    checkTrue(!is(res, "try-error"), msg="iterators stress test failed")    
}

"testzzz.restore.session" <- function()
{
    checkTrue(RevoIOQ:::restoreRUnitSession(session), msg="Session restoration failed")
}


