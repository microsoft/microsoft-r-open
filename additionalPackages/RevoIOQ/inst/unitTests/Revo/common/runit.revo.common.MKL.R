## runit.revo.common.MKL.R -- Unit Test for Hypothesis Testing
## Copyright (c) 2016 Microsoft Corporation All Rights Reserved
##
## This test is part of the Micrsoft R Server Validation Suite

test.dgemm.crossprod <- function(){
	
    loaded.packages <- search()
    if ("package:RevoUtilsMath" %in% loaded.packages)
    {
	 mkl.loaded <- TRUE
    } else {
       mkl.loaded <- FALSE
    }    
 
    if (!mkl.loaded)
    {
    	 DEACTIVATED("Deactivating test because Intel MKL libraries are not loaded.")	
    }

    num.cores.avail <- getMKLthreads()    
    mkl.threads.to.use <- rep(0,3)

    if( num.cores.avail < 4)
    {
    	 mkl.threads.to.use <- c(1,2)
    }  else {
       	mkl.threads.to.use <- c(1,2,4)
       mkl.threads.to.use
    }

    n <- 5184
    m <- 72
    range.vec <- vector(mode = "integer", length = 0)
    range.vec.expected <- rep(0, (length(mkl.threads.to.use) * 2))
    
    for (i in 1:length(mkl.threads.to.use))
    {
        setMKLthreads(mkl.threads.to.use[i])
	  P <- matrix(runif(n*m), n, m)
        N <- matrix(rnorm(n*n), n, n)
        F <- matrix(rnorm(m*n), m, n)
        F2 <- F
        N2 <- N
        P2 <- P
        # Compute matrix crossproduct
        R <- F %*% (N %*% P)
        Rb <- F2 %*% (N2 %*% P2)
        range.diff <- range(R-Rb)  
        range.vec <- c(range.vec, range.diff)
    }

    # Set number of times to run same crossproduct calculation.
    num.runs <- 5
    cprod.calc <- vector(mode = "double", length = num.runs)
    for (j in 1:num.runs)
    {
        
	cprod.calc[j] <- (F %*% (N %*% P))[1,1]
      assign("cprod.calc", cprod.calc, envir = parent.frame(1))
    } 

    # Check for equality in range between matrices.     
    if (checkEquals(range.vec.expected, range.vec))
    {
        print("Test for matrix equality passed.")
    }   else {
	  print("Test for matrix equality FAILED.")
    }

    # Check end result of repeated crossproduct calculation. 
    # If elements are equal, variance is zero.
    
    var.cprod <- var(cprod.calc)
    if (checkEquals(var.cprod, 0))
    {
    	 print("Test for zero variance passed.")
    }  else {
       print("Test for zero variance failed.")
    }

}

