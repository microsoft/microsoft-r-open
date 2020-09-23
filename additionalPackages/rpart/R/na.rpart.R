na.rpart <- function(x)
{
    Terms <- attr(x, "terms")
    if (!is.null(Terms)) yvar <- attr(Terms, "response") else yvar <- 0L
    if (yvar == 0L) {
        xmiss <- is.na(x)
        keep <- (xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)
    } else {
        xmiss <- is.na(x[-yvar])
        ymiss <- is.na(x[[yvar]])
        keep <- if (is.matrix(ymiss))
            ((xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)) &
                ((ymiss %*% rep(1, ncol(ymiss))) == 0)
        else ((xmiss %*% rep(1, ncol(xmiss))) < ncol(xmiss)) & !ymiss
    }
    if (all(keep)) x
    else {
	temp <- seq(keep)[!keep]
	names(temp) <- row.names(x)[!keep]
        ## the methods for this group are all the same as for na.omit
	class(temp) <- c("na.rpart", "omit")
	structure(x[keep , , drop = FALSE], na.action = temp)
    }
}
