## Contributed by B.D. Ripley 97/07/17
##
plotcp <- function(x, minline = TRUE, lty = 3, col = 1,
		   upper = c("size", "splits", "none"), ...)
{
    dots <- list(...)
    if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")
    upper <- match.arg(upper)
    p.rpart <- x$cptable
    if (ncol(p.rpart) < 5L)
        stop("'cptable' does not contain cross-validation results")
    xstd <- p.rpart[, 5L]
    xerror <- p.rpart[, 4L]
    nsplit <- p.rpart[, 2L]
    ns <- seq_along(nsplit)
    cp0 <- p.rpart[, 1L]
    cp <- sqrt(cp0 * c(Inf, cp0[-length(cp0)]))
    if (! "ylim" %in% names(dots))
        dots$ylim <- c(min(xerror - xstd) - 0.1, max(xerror + xstd) + 0.1)
    do.call(plot, c(list(ns, xerror, axes = FALSE, xlab = "cp",
                         ylab = "X-val Relative Error", type = "o"), dots))
    box()
    axis(2, ...)
    segments(ns, xerror - xstd, ns, xerror + xstd)
    axis(1L, at = ns, labels = as.character(signif(cp, 2L)), ...)
    switch(upper,
           size = {
               axis(3L, at = ns, labels = as.character(nsplit + 1), ...)
               mtext("size of tree", side = 3, line = 3)
           },
           splits = {
               axis(3L, at = ns, labels = as.character(nsplit), ...)
               mtext("number of splits", side = 3, line = 3)
           })
    minpos <- min(seq_along(xerror)[xerror == min(xerror)])
    if (minline) abline(h = (xerror + xstd)[minpos], lty = lty, col = col)
    invisible()
}
