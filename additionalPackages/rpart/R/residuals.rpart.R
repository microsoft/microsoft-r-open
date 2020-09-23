residuals.rpart <-
    function(object, type = c("usual", "pearson", "deviance"), ...)
{
    if (!inherits(object, "rpart"))
        stop("Not a legitimate \"rpart\" object")

    y <- object$y
    if (is.null(y)) y <- model.extract(model.frame(object), "response")
    frame <- object$frame
    type <- match.arg(type)
    if (is.na(match(type, c("usual", "pearson", "deviance"))))
        stop("Invalid type of residual")

    resid <- if (object$method == "class") {
	ylevels <- attr(object, "ylevels")
	nclass <- length(ylevels)

        if (type == "usual") {
            yhat <- frame$yval[object$where]
            loss <- object$parms$loss
        } else {
	    yprob <- frame$yval2[object$where, 1L + nclass + 1L:nclass]
	    yhat <- yprob[cbind(seq(y), unclass(y))]
        }
        switch(type,
               usual = loss[cbind(y, yhat)],
               pearson = (1 - yhat)/yhat,
               deviance = -2 * log(yhat))
    } else if (object$method == "poisson" || object$method == "exp") {
	lambda <- (object$frame$yval)[object$where]
	time <- y[, 1L]               # observation time in new data
	events <- y[, 2L]               # number of events, in new data
	expect <- lambda * time         # expected number of events
	temp <- ifelse(expect == 0, 0.0001, 0) # failsafe for log(0)

	switch(type,
               usual = events - expect,
               pearson = (events - expect)/sqrt(temp),
               deviance = sign(events- expect) *
                   sqrt(2 * (events * log(events/temp) - (events - expect)))
               )
    } else
        y - frame$yval[object$where]

    names(resid) <- names(y)
    ## Expand out the missing values in the result
    if (!is.null(object$na.action)) naresid(object$na.action, resid) else resid
}
