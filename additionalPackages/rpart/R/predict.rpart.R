predict.rpart <- function(object, newdata,
	 type = c("vector", "prob", "class", "matrix"),
         na.action = na.pass, ...)
{
    if (!inherits(object, "rpart")) stop("Not a legitimate \"rpart\" object")

    mtype <- missing(type)
    type <- match.arg(type)
    where <- if (missing(newdata)) object$where
    else {
	if (is.null(attr(newdata, "terms"))) {
	    Terms <- delete.response(object$terms)
	    newdata <- model.frame(Terms, newdata, na.action = na.action,
                                   xlev = attr(object, "xlevels"))
            if (!is.null(cl <- attr(Terms, "dataClasses")))
                .checkMFClasses(cl, newdata, TRUE)
        }
	pred.rpart(object, rpart.matrix(newdata))
    }
    frame <- object$frame
    ylevels <- attr(object, "ylevels")
    nclass <- length(ylevels)
    if (mtype && nclass > 0L) type <- "prob"
    if (type == "vector" || (type == "matrix" && is.null(frame$yval2))) {
	pred <- frame$yval[where]
	names(pred) <- names(where)
    } else if (type == "matrix") {
	pred <- frame$yval2[where, ]
	dimnames(pred) <- list(names(where), NULL)
    } else if (type == "class" && nclass > 0L) {
	if (length(ylevels) == 0L)
            stop("type 'class' is only appropriate for classification")
	pred <- factor(ylevels[frame$yval[where]], levels = ylevels)
	names(pred) <- names(where)
    } else if (type == "prob" && nclass > 0L) {
	pred <- frame$yval2[where, 1L + nclass + 1L:nclass, drop = FALSE]
	dimnames(pred) <- list(names(where), ylevels)
    } else stop("Invalid prediction for \"rpart\" object")

    # Expand out the missing values in the result
    # But only if operating on the original dataset
    if (missing(newdata) && !is.null(object$na.action))
        pred <- naresid(object$na.action, pred)
    pred
}

