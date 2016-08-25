
## Our version of the fix function for use in the IDE

revoFix <- function (x, ...,edit1DDataAsDataFrame=TRUE, editTSAsDataFrame=TRUE) 
{ 
    subx <- substitute(x) 
    if (is.name(subx)) 
        subx <- deparse(subx) 
    if (!is.character(subx) || length(subx) != 1) 
        stop("'fix' requires a name") 
    parent <- parent.frame()
    isTS <- FALSE 
    if (exists(subx, envir = parent, inherits = TRUE)) { 
        worx <- get(subx, envir=parent) 
        classworx <- class(worx) 
	  isTS <- is(worx, "ts")
	  if (isTS && editTSAsDataFrame==TRUE){
			timeworx <- stats:::time(worx)
			tspworx <- attributes(worx)$tsp
			if (is.null(dim(worx))){
				worx <- data.frame(as.vector(worx))
				names(worx) <- subx
			} else {
			    worx <- data.frame(data.matrix(worx), check.names=FALSE)
			}		
        } 
     if (edit1DDataAsDataFrame){ 
		# any other atomic-object specific handling must be done before this next bit
            if((is.atomic(worx) && is.null(dim(worx))) || is.factor(worx)) { 
				worx <- data.frame(worx, stringsAsFactors=FALSE) 
				names(worx) <- subx
			}    
		}
        x <- edit(worx, title = subx, ...) 
    } 
    else { 
        x <- edit(function() { 
        }, title = subx, ...) 
        environment(x) <- .GlobalEnv 
    } 
	if (isTS) {
		if (ncol(x)==1) {
			x <- x[, 1]
		} else {
			x <- data.matrix(x)
		}
        x <- ts(x, start = tspworx[1], end = tspworx[2], frequency = tspworx[3])
	}
    if (class(x) != classworx && ncol(x)==1) { 
		x <- x[,1] 
        } 
    assign(subx, x, envir = .GlobalEnv) 
} 

