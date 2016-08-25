
## Our version of the plot function for use in the IDE

revoPlot <- function(x, ...)
{
    UseMethod("revoPlot")
}
revoPlot.default <- function(x, ...)
{
    do.call(graphics::plot, list(substitute(x), ...))
}
revoPlot.ts <- function(x, ...) 
{
    if (!hasArg(col)) {
        do.call(stats::plot.ts, list(substitute(x), col=lattice::trellis.par.get("plot.line")$col,...))
    } else {
        do.call(stats::plot.ts, list(substitute(x), ...))
    }
}

revoPlot.data.frame <- function(x, ...) 
{
    if (!is.data.frame(x)){
        stop("'revoPlot.data.frame' applied to non data frame")
    }
    if (ncol(x) > 2){
        lattice:::splom(data.matrix(x), ...)
    } else {
        graphics:::plot.data.frame(x, ...)	    
    }
}
revoPlot.matrix <- function(x, ...) {
    if (!is.matrix(x)) {
        stop("'revoPlot.matrix' applied to non matrix")
    }
    if (ncol(x) > 2) {
        lattice:::splom(data.matrix(x), data=NULL, ...)
    } else {
	  graphics::plot(x, ...)
    }          
}
	