if(!nzchar(Sys.getenv("RODBC_TESTING"))) q("no")

runone <- function(f)
{
    message("  Running ", sQuote(f))
    infile <- paste(f, "RR", sep = ".")
    outfile <- paste(f, "Rout", sep = ".")
    cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                 "CMD BATCH --vanilla",
                 shQuote(infile), shQuote(outfile))
    res <- system(cmd)
    if (res) {
        cat(readLines(outfile), sep="\n")
        file.rename(outfile, paste(outfile, "fail", sep="."))
        return(1L)
    }
    savefile <- paste(outfile, "save", sep = "." )
    if (file.exists(savefile)) {
        message("  Comparing ", sQuote(outfile), " to ",
                sQuote(savefile), " ...", appendLF = FALSE)
        res <- tools:::Rdiff(outfile, savefile, TRUE)
        if (!res) message(" OK")
    }
    0L
}


res <- if(.Platform$OS.type == "windows")
    runone("mysql-win") else runone("mysql")
res <- res + runone("sqlite3")
res <- res + runone("postgresql")
if(.Platform$OS.type == "windows") {
    res <- res + runone("access")
    res <- res + runone("excel")
    res <- res + runone("SQLServer")
    res <- res + runone("mimer")
    res <- res + runone("DB2")
    res <- res + runone("Oracle")
}

proc.time()

if(res) stop(gettextf("%d tests failed", res))
