##
##  Get a set of cross-validated predictions
##  Most of the setup is identical to the rpart routine
##
xpred.rpart <- function(fit, xval = 10L, cp, return.all = FALSE)
{
    if (!inherits(fit, "rpart")) stop("Invalid fit object")

    method <- fit$method
    method.int <- pmatch(method, c("anova", "poisson", "class", "user", "exp"))
    if (method.int == 5L) method.int <- 2L
    Terms <- fit$terms
    Y <- fit$y
    X <- fit$x
    wt <- fit$wt
    numresp <- fit$numresp

    if (is.null(Y) || is.null(X)) {
	m <- fit$model
	if (is.null(m)) {
	    m <- fit$call[match(c("", "formula", "data", "weights", "subset",
                                  "na.action"), names(fit$call), 0L)]
	    if (is.null(m$na.action)) m$na.action <- na.rpart
	    m[[1]] <- quote(stats::model.frame)
	    m <- eval.parent(m)
        }
	if (is.null(X)) X <- rpart.matrix(m)
	if (is.null(wt)) wt <- model.extract(m, "weights")
	if (is.null(Y)) {
	    yflag <- TRUE
	    Y <- model.extract(m, "response")
            offset <- attr(Terms, "offset")
	    if (method != "user") {
		init <- get(paste("rpart", method, sep = "."))(Y, offset, NULL)
		Y <- init$y
		numy <- if (is.matrix(Y)) ncol(Y) else 1L
            }
        } else {
	    yflag <- FALSE
            numy <- if (is.matrix(Y)) ncol(Y) else 1L
        }
    } else {
	yflag <- FALSE
        numy <- if (is.matrix(Y)) ncol(Y) else 1L
	offset <- 0L
    }

    nobs <- nrow(X)
    nvar <- ncol(X)
    if (length(wt) == 0) wt <- rep(1, nobs)

    cats <- rep(0, nvar)
    xlevels <- attr(fit, "xlevels")
    if (!is.null(xlevels))
        cats[match(names(xlevels), colnames(X))] <-
            unlist(lapply(xlevels, length))

    controls <- fit$control
    if (missing(cp)) {
	cp <- fit$cptable[, 1L]
	cp <- sqrt(cp * c(10, cp[-length(cp)]))
	cp[1L] <- (1 + fit$cptable[1L, 1L])/2
    }
#    ncp <- length(cp)

    if (length(xval) == 1L) {
	## make random groups
	xgroups <- sample(rep(1L:xval, length = nobs), nobs, replace = FALSE)
    } else if (length(xval) == nrow(X)) {
	xgroups <- xval
	xval <- length(unique(xgroups))
    } else {
	## Check to see if observations were removed due to missing
	if (!is.null(fit$na.action)) {
	    ## if na.rpart was used, then na.action will be a vector
	    temp <- as.integer(fit$na.action)
	    xval <- xval[-temp]
	    if (length(xval) == nobs) {
		xgroups <- xval
		xval <- length(unique(xgroups))
            } else stop("Wrong length for 'xval'")
        } else stop("Wrong length for 'xval'")
    }

    costs <- fit$call$costs
    if (is.null(costs)) costs <- rep(1, nvar)

    parms <- fit$parms
    if (method == "user") {
	mlist <- fit$functions

	## If yflag == TRUE, then y was retrieved from the original data and we
        ##   need to call init to check and possibly transform it.
        ## If FALSE, then we have one of the few differences with the
        ##  rpart setup
        if (yflag) {
            init <- if (length(parms) == 0L) mlist$init(Y, offset, , wt)
                    else mlist$init(Y, offset, parms, wt)
            Y <- init$Y
            numy <- init$numy
            parms <- init$parms
        } else {
            numy <- if (is.matrix(Y)) ncol(Y) else 1L
            init <- list(numresp = numresp, numy = numy, parms = parms)
        }
        keep <- rpartcallback(mlist, nobs, init) # assign to a variable to
                                                 # stop garbage collection
	method.int <- 4L # the fourth entry in func_table.h
    }

    ## Finally do the work
    if (is.matrix(Y))  Y <- as.double(t(Y)) else storage.mode(Y) <- "double"
    storage.mode(X) <- "double"
    storage.mode(wt) <- "double"
    temp <- as.double(unlist(parms))
    if (length(temp) == 0L) temp <- 0    # if NULL, pass a dummy
    pred <- .Call(C_xpred,
                  ncat = as.integer(cats * !fit$ordered),
                  method = as.integer(method.int),
                  as.double(unlist(controls)),
                  temp,
                  as.integer(xval),
                  as.integer(xgroups),
                  Y,
                  X,
                  wt,
                  as.integer(numy),
                  as.double(costs),
                  as.integer(return.all),
                  as.double(cp),
                  as.double(fit$frame[1L, "dev"]),
                  as.integer(numresp))

    if (return.all && numresp > 1L) {
        temp <- array(pred, dim = c(numresp, length(cp), nrow(X)),
                      dimnames = list(NULL, format(cp), rownames(X)))
        aperm(temp)                     # flip the dimensions
    } else
        matrix(pred, nrow = nrow(X), byrow = TRUE,
               dimnames = list(rownames(X), format(cp)))
}
