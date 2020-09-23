.onUnload <- function(libpath) library.dynam.unload("rpart", libpath)

.noGenerics <- TRUE

tree.depth <- function (nodes)
{
    depth <- floor(log(nodes, base = 2) + 1e-7)
    depth - min(depth)
}

string.bounding.box <- function(s)
{
    s2 <- strsplit(s, "\n")
    rows <- sapply(s2, length)
    columns <- sapply(s2, function(x) max(nchar(x, "w")))
    list(columns = columns, rows = rows)
}

node.match <- function(nodes, nodelist, leaves, print.it = TRUE)
{
    node.index <- match(nodes, nodelist, 0L)
    bad <- nodes[node.index == 0L]
    ## FIXME: plurals?
    if (length(bad) > 0 && print.it)
        warning(gettextf("supplied nodes %s are not in this tree",
                         paste(bad, collapse = ",")), domain = NA)
    good <- nodes[node.index > 0L]
    if (!missing(leaves) && any(leaves <- leaves[node.index])) {
        warning(gettextf("supplied nodes %s are leaves",
                paste(good[leaves], collapse = ",")), domain = NA)
        node.index[node.index > 0L][!leaves]
    } else node.index[node.index > 0L]
}

descendants <- function(nodes, include = TRUE)
{
    n <- length(nodes)
    if (n == 1L) return(matrix(TRUE, 1L, 1L))
    ind <- 1:n
    desc <- matrix(FALSE, n, n)
    if (include) diag(desc) <- TRUE
    parents <- match((nodes %/% 2L), nodes)
    lev <- floor(log(nodes, base = 2))
    desc[1L, 2L:n] <- TRUE
    for (i in max(lev):2L) {
        desc[cbind(ind[parents[lev == i]], ind[lev == i])] <- TRUE
        parents[lev == i] <- parents[parents[lev == i]]
        lev[lev == i] <- i - 1L
    }
    desc
}

rpart_env <- new.env()
