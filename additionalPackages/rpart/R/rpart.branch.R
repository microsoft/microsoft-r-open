##
## Compute the "branches" to be drawn for an rpart object
##
## most likely this could simply default to branch = 1
rpart.branch <- function(x, y, node, branch)
{
    if (missing(branch)) {
        pn <- paste0("device", dev.cur())
        if (!exists(pn, envir = rpart_env, inherits = FALSE))
            stop("no information available on parameters from previous call to plot()")
        parms <- get(pn, envir = rpart_env, inherits = FALSE)
        branch <- parms$branch
    }

    ## Draw a series of horseshoes, left son, up, over, down to right son
    ##   NA's in the vector cause lines() to "lift the pen"
    is.left <- (node %% 2L == 0L)            #left hand sons
    node.left <- node[is.left]
    parent <- match(node.left/2L, node)
    sibling <- match(node.left + 1L, node)
    temp <- (x[sibling] - x[is.left]) * (1 - branch)/2
    xx <- rbind(x[is.left], x[is.left] + temp,
                x[sibling] - temp, x[sibling], NA)
    yy <- rbind(y[is.left], y[parent], y[parent], y[sibling], NA)
    list(x = xx, y = yy)
}
