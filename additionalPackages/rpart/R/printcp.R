## print out the cptable, along with some summary of the tree
printcp <- function(x, digits = getOption("digits") - 2L)
{
    if (!inherits(x, "rpart")) stop ("'x' must be an \"rpart\" object")
    cat(switch(x$method,
               anova = "\nRegression tree:\n" ,
               class = "\nClassification tree:\n" ,
               poisson = "\nRates regression tree:\n",
               exp = "\nSurvival regression tree:\n")
        )

    if (!is.null(cl <- x$call)) {
	dput(cl, control = NULL)
	cat("\n")
    }
    frame <- x$frame
    leaves <- frame$var == "<leaf>"
    used <- unique(frame$var[!leaves])

    if (!is.null(used)) {
        cat("Variables actually used in tree construction:\n")
        print(sort(as.character(used)), quote = FALSE)
        cat("\n")
    }


    cat("Root node error: ", format(frame$dev[1L], digits = digits), "/",
        frame$n[1L], " = ",
        format(frame$dev[1L]/frame$n[1L], digits = digits),
        "\n\n", sep = "")


    n <- x$frame$n
    omit <- x$na.action
    if (length(omit)) cat("n=", n[1L], " (", naprint(omit), ")\n\n", sep = "")
    else cat("n=", n[1L], "\n\n")
    print(x$cptable, digits = digits)
    invisible(x$cptable)
}
