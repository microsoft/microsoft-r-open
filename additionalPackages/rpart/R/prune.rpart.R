prune.rpart <- function(tree, cp, ...)
{
    ff <- tree$frame
    id <- as.integer(row.names(ff))
    toss <- id[ff$complexity <= cp & ff$var != "<leaf>"] #not a leaf
    if (length(toss) == 0L) return(tree)   # all the tree is retained
    newx <- snip.rpart(tree, toss)
    ## Now cut down the CP table
    temp <- pmax(tree$cptable[, 1L], cp)
    keep <- match(unique(temp), temp)
    newx$cptable <- tree$cptable[keep, , drop = FALSE]
    newx$cptable[max(keep), 1L] <- cp
    # Reset the variable importance
    newx$variable.importance <- importance(newx)
    newx
}
