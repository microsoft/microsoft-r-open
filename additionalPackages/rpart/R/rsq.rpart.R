## This function plots the approximate r-square for the different
## splits (assumes using anova method).

rsq.rpart <- function(x)
{

    if (!inherits(x, "rpart")) stop("Not a legitimate \"rpart\" object")

    p.rpart <- printcp(x)
    xstd <- p.rpart[, 5L]
    xerror <- p.rpart[, 4L]
    rel.error <- p.rpart[, 3L]
    nsplit <- p.rpart[, 2L]
    method <- x$method

    if (!method == "anova")
        warning("may not be applicable for this method")
    plot(nsplit, 1 - rel.error, xlab = "Number of Splits", ylab = "R-square",
        ylim = c(0, 1), type = "o")
    par(new = TRUE)
    plot(nsplit, 1 - xerror, type = "o", ylim = c(0, 1), lty = 2,
        xlab = " ", ylab = " ")
    legend(0, 1, c("Apparent", "X Relative"), lty = 1:2)
    ylim <- c(min(xerror - xstd) - 0.1, max(xerror + xstd) +
        0.1)
    plot(nsplit, xerror, xlab = "Number of Splits", ylab = "X Relative Error",
        ylim = ylim, type = "o")
    segments(nsplit, xerror - xstd, nsplit, xerror + xstd)
    invisible()
}
