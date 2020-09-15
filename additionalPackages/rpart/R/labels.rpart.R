## Make the nice labels used by print and summary
##   digits = obvious
##   minlength = 0 = don't abbrev factors
##               1 = use single letters
##               2+= the same arg as the "abbreviate" function
##   collapse = an oddly named argument
##              FALSE = return a matrix with two columns, containing the labels of
##                    the left and right descendants of each node
##              TRUE = return a vector of 1 column, with the label of the parent
##   pretty: for historical compatability
##               0   -> minlength = 0
##              NULL -> minlength = 1
##               TRUE   -> minlength = 4
##   ... = other args for abbreviate()
##
labels.rpart <- function(object, digits = 4, minlength = 1L, pretty,
                         collapse = TRUE, ...)
{
    if (missing(minlength) && !missing(pretty)) {
	minlength <- if (is.null(pretty)) 1L
	else if (is.logical(pretty)) {
	    if (pretty) 4L else 0L
        } else 0L
    }

    ff <- object$frame
    n <- nrow(ff)
    if (n == 1L) return("root")            # special case of no splits

    is.leaf <- (ff$var == "<leaf>")
    whichrow <- !is.leaf
    vnames <- ff$var[whichrow] # the variable names for the primary splits

    index <- cumsum(c(1, ff$ncompete + ff$nsurrogate + !is.leaf))
    irow <- index[c(whichrow, FALSE)] # we only care about the primary split
    ncat <- object$splits[irow, 2L]

    ## Now to work: first create labels for the left and right splits,
    ##  but not for leaves of course
    ##
    lsplit <- rsplit <- character(length(irow))

    if (any(ncat < 2L)) {               # any continuous vars ?
	jrow <- irow[ncat < 2L]
	cutpoint <- formatg(object$splits[jrow, 4L], digits)
	temp1 <- (ifelse(ncat < 0, "< ", ">="))[ncat < 2L]
	temp2 <- (ifelse(ncat < 0, ">=", "< "))[ncat < 2L]
	lsplit[ncat<2L] <- paste0(temp1, cutpoint)
	rsplit[ncat<2L] <- paste0(temp2, cutpoint)
    }

    if (any(ncat > 1L)) {               # any categorical variables ?
	xlevels <- attr(object, "xlevels")
	##
	## jrow will be the row numbers of factors within lsplit and rsplit
	## crow the row number in "csplit"
	## and cindex the index on the "xlevels" list
	##
	jrow <- seq_along(ncat)[ncat > 1L]
	crow <- object$splits[irow[ncat > 1L], 4L] #row number in csplit
	cindex <- (match(vnames, names(xlevels)))[ncat > 1L]

	## Now, abbreviate the levels
	if (minlength == 1L) {
	    if (any(ncat > 52L))
		warning("more than 52 levels in a predicting factor, truncated for printout",
                        domain = NA)
	    xlevels <- lapply(xlevels, function(z) c(letters, LETTERS)[pmin(seq_along(z), 52L)])
        } else if (minlength > 1L)
	    xlevels <- lapply(xlevels, abbreviate, minlength, ...)

	## Now tuck in the labels
	## I'll let some other clever person vectorize this
	for (i in seq_along(jrow)) {
	    j <- jrow[i]
	    splits <- object$csplit[crow[i], ]
	    ## splits will contain 1=left, 3=right, 2= neither
            cl <- if (minlength == 1L) "" else ","
            lsplit[j] <-
                paste((xlevels[[cindex[i]]])[splits == 1L], collapse = cl)
            rsplit[j] <-
                paste((xlevels[[cindex[i]]])[splits == 3L], collapse = cl)
        }
    }

    if (!collapse) {  # called by no routines that I know of
	ltemp <- rtemp <- rep("<leaf>", n)
	ltemp[whichrow] <- lsplit
	rtemp[whichrow] <- rsplit
	return(cbind(ltemp, rtemp))
    }

    lsplit <- paste0(ifelse(ncat < 2L, "", "="), lsplit)
    rsplit <- paste0(ifelse(ncat < 2L, "", "="), rsplit)

    ## Now match them up to node numbers
    ##   The output will have one label per row of object$frame, each
    ##   corresponding the the line segement joining this node to its parent
    varname <- (as.character(vnames))
    node <- as.numeric(row.names(ff))
    parent <- match(node %/% 2L, node[whichrow])
    odd <- (as.logical(node %% 2L))

    labels <- character(n)
    labels[odd] <- paste0(varname[parent[odd]], rsplit[parent[odd]])
    labels[!odd] <- paste0(varname[parent[!odd]], lsplit[parent[!odd]])
    labels[1L] <- "root"
    labels
}
