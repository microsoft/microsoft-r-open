##
##  This routine "throws away" branches
##
snip.rpart <- function(x, toss)
{
    if (!inherits(x, "rpart")) stop("Not an \"rpart\" object")

    if (missing(toss) || length(toss) == 0L) {
        toss <- snip.rpart.mouse(x)
	if (length(toss) == 0L) return(x)
    }

    ff <- x$frame
    id <- as.integer(row.names(ff))
    ff.n <- length(id)
    toss <- unique(toss)
    toss.idx <- match(toss, id, 0L) # the rows of the named nodes
    if (any(toss.idx == 0L)) {
        ## FIXME: plural?
        warning(gettext("Nodes %s are not in this tree", toss[toss.idx == 0L]),
                domain = NA)
	toss <- toss[toss.idx > 0L]
        toss.idx <- toss.idx[toss.idx > 0L]
    }

    ##    if (any(toss==1))  {
    ##	## a special case that causes grief later
    ##	warning("Can't prune away the root node and still have a tree!")
    ##        return(NULL)
    ##	}

    ## Now add all of the descendants of the selected nodes
    ##   We do this be finding all node's parents.
    ##        (Division by 2 gives the parent of any node.)
    ##   At each step we make id2 <- parent(id2), and augment 'toss' with
    ##     found children.  The loop should take <  log_2(maxdepth)/2 steps
    id2 <- id
    while (any(id2 > 1L)) {
	id2 <- id2 %/% 2L
	xx <- (match(id2, toss, 0L) > 0L)
	toss <- c(toss, id[xx])
        id2[xx] <- 0L
    }

    ## Now "toss" contains all of the nodes that should not be splits
    temp <- match(toss %/% 2L , toss, 0L) # which are leaves?
    newleaf <- match(toss[temp == 0L], id)       # row numbers, leaves
    keepit <- (1:ff.n)[is.na(match(id, toss))] # row numbers to be let be

    ## Compute the parent row for each row in the splits structure
    ##  Then "thin out" the splits and csplit components
    n.split <- rep(1L:ff.n, ff$ncompete + ff$nsurrogate + (ff$var != "<leaf>"))
    split <- x$splits[match(n.split, keepit, 0L) > 0L, , drop = FALSE]
    temp <- split[, 2L] > 1L           # which rows point to categoricals?
    if (any(temp)) {
        x$csplit <- x$csplit[split[temp, 4L], , drop = FALSE]
	split[temp, 4L] <- 1L
        if (is.matrix(x$csplit)) split[temp, 4L] <- 1L:nrow(x$csplit)
    } else x$csplit <- NULL
    x$splits <- split

    ## Thin out unneeded rows in the frame component
    ff$ncompete[newleaf] <- ff$nsurrogate[newleaf] <- 0L
    ff$var[newleaf] <- "<leaf>"
    x$frame <- ff[sort(c(keepit, newleaf)), ]

    ## Now do the 'parents' loop one more time, to fix up the "where" vector
    ## This pass requires log_2(depth) iterations
    ##
    id2 <- id[x$where]                  # the list of old leaf nodes
    id3 <- id[sort(c(keepit, newleaf))]
    temp <- match(id2, id3, 0L)
    while (any(temp == 0L)) {
	id2[temp == 0L] <- id2[temp == 0L] %/% 2L
	temp <- match(id2, id3, 0L)
    }
    x$where <- match(id2, id3)

    x
}
