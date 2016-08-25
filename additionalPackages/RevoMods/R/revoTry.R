# based on the R 'try' function but turns interrupts into errors and returns them - this allows us to properly handle CTRL-BREAK
"revoTry" <- function (expr, silent = FALSE) 
{
    tryCatch(expr, error = function(e) {
        call <- conditionCall(e)
        if (!is.null(call)) {
            if (identical(call[[1L]], quote(doTryCatch))) 
                call <- sys.call(-4L)
            dcall <- deparse(call)[1L]
            prefix <- paste("Error in", dcall, ": ")
            LONG <- 75L
            msg <- conditionMessage(e)
            sm <- strsplit(msg, "\n")[[1L]]
            w <- 14L + nchar(dcall, type = "w") + nchar(sm[1L], 
                type = "w")
            if (is.na(w)) 
                w <- 14L + nchar(dcall, type = "b") + nchar(sm[1L], 
                  type = "b")
            if (w > LONG) 
                prefix <- paste(prefix, "\n  ", sep = "")
        }
        else prefix <- "Error : "
        msg <- paste(prefix, conditionMessage(e), "\n", sep = "")
        .Internal(seterrmessage(msg[1L]))
        if (!silent && identical(getOption("show.error.messages"), 
            TRUE)) {
            cat(msg, file = stderr())
            .Internal(printDeferredWarnings())
        }
        invisible(structure(msg, class = "try-error"))
    },
	interrupt = function(e) {invisible(structure("User interrupted execution.", class = "try-error"))})
}	
