##
##  Interactively snip off part of a tree
##
snip.rpart.mouse <- function(tree, parms)
{
    if (missing(parms)) {
        pn <- paste0("device", dev.cur())
        if (!exists(pn, envir = rpart_env, inherits = FALSE))
            stop("no information available on parameters from previous call to plot()")
        parms <- get(pn, envir = rpart_env, inherits = FALSE)
    }


    xy <- rpartco(tree, parms)
    toss <- NULL
    ff <- tree$frame
    if (length(parms$branch))
	branch <- parms$branch
    else branch <- 1L

    node <- as.integer(row.names(tree$frame))
    draw <- rpart.branch(xy$x, xy$y, node, branch)

    lastchoice <- 0L
    while (length(choose <- identify(xy, n = 1L, plot = FALSE))) {
	if (ff$var[choose] == "<leaf>") {
            cat("Terminal node -- try again\n")
            next
        }

	if (choose != lastchoice) {
	    ## print out some info on the click
	    cat("node number:", node[choose], " n=", ff$n[choose], "\n")
	    cat("    response=", format(ff$yval[choose]))
	    if (is.null(ff$yval2)) cat ("\n")
	    else if (is.matrix(ff$yval2))
                cat(" (", format(ff$yval2[choose, ]), ")\n")
	    else  cat(" (", format(ff$yval2[choose]), ")\n")
	    cat("    Error (dev) = ", format(ff$dev[choose]), "\n")
	    lastchoice <- choose
        } else {
	    ## second click-- erase all of the descendants
	    ##   (stolen from snip.tree)
	    id <- node[choose]
	    id2 <- node
	    while (any(id2 > 1L)) {
		id2 <- id2 %/% 2L
		temp <- match(id2, id, 0L) > 0L
  	        id <- c(id, node[temp])
		id2[temp] <- 0L
            }
	    temp <- match(id, node[ff$var != "<leaf>"], 0L)
	    lines(c(draw$x[, temp]), c(draw$y[, temp]), col = 0L)
	    toss <- c(toss, node[choose])
        }
    }
    toss
}
