timestamp <- function (stamp = date(), prefix = "##------ ", suffix = " ------##", quiet = FALSE)
{
    stamp <- paste0(prefix, stamp, suffix)


    # if not running in the Revolution IDE
    if (commandArgs()[1] != "" || !interactive()) 
    {
        .External2(utils:::C_addhistory, stamp)

    }

    if (!quiet)
        cat(stamp, sep = "\n")
    invisible(stamp)
}