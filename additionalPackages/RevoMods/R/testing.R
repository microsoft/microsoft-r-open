#  File src/library/tools/R/testing.R
#  Part of the R package, http://www.R-project.org
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/

## functions principally for testing R and packages

testInstalledPackage <-
    function(pkg, lib.loc = NULL, outDir = ".",
             types = c("examples", "tests"),
             srcdir = NULL, Ropts = "")
{
    types <- pmatch(types, c("examples", "tests"))
    pkgdir <- find.package(pkg, lib.loc)
    exdir <- file.path(pkgdir, "R-ex")
    owd <- setwd(outDir)
    on.exit(setwd(owd))
    strict <- as.logical(Sys.getenv("R_STRICT_PACKAGE_CHECK", "FALSE"))

    if (1 %in% types) {
        message("Testing examples for package ", sQuote(pkg))
        Rfile <- .createExdotR(pkg, pkgdir, silent = TRUE)
        if (length(Rfile)) {
            outfile <- paste(pkg, "-Ex.Rout", sep = "")
            failfile <- paste(outfile, "fail", sep = "." )
            savefile <- paste(outfile, "prev", sep = "." )
            if (file.exists(outfile)) file.rename(outfile, savefile)
            unlink(failfile)
            ## Create as .fail in case this R session gets killed
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --no-timing", Ropts,
                         shQuote(Rfile), shQuote(failfile))
            if (.Platform$OS.type == "windows") Sys.setenv(R_LIBS="")
            else cmd <- paste("R_LIBS=", cmd)
            res <- system(cmd)
            if (res) return(invisible(1L)) 
            else
            {
                Sys.sleep(10)
                file.rename(failfile, outfile)
            }
            

            savefile <- paste(outfile, "save", sep = "." )
            if (!is.null(srcdir)) savefile <- file.path(srcdir, savefile)
            else {
                tfile <- file.path(pkgdir, "tests", "Examples" , savefile)
                if(!file.exists(savefile) && file.exists(tfile))
                    savefile <- tfile
            }
            if (file.exists(savefile)) {
               if (file.exists(savefile)) {
                    message("  comparing ", sQuote(outfile), " to ",
                            sQuote(basename(savefile)), " ...", appendLF = FALSE)
                    res <- tools::Rdiff(outfile, savefile)
                    if (!res) message(" OK")
                    else if(strict)
                        stop("  ", "results differ from reference results")
                }
            } else {
                prevfile <- paste(outfile, "prev", sep = "." )
                if (file.exists(prevfile)) {
                    message("  comparing ", sQuote(outfile), " to ",
                            sQuote(basename(prevfile)), " ...", appendLF = FALSE)
                    res <- tools::Rdiff(outfile, prevfile)
                    if (!res) message(" OK")
                }
            }
        } else
            warning(gettextf("no examples found for package %s", sQuote(pkg)),
                    call. = FALSE, domain = NA)
    }

    ## FIXME merge with code in .runPackageTests
    if (2 %in% types && file_test("-d", d <- file.path(pkgdir, "tests"))) {
        this <- paste(pkg, "tests", sep="-")
        unlink(this, recursive = TRUE)
        dir.create(this)
        ## system(paste("cp -pr", file.path(d, "*"), this))
        file.copy(Sys.glob(file.path(d, "*")), this, recursive = TRUE)
        setwd(this)
        message("Running specific tests for package ", sQuote(pkg))
        Rfiles <- dir(".", pattern="\\.R$")
        for(f in Rfiles) {
            message("  Running ", sQuote(f))
            outfile <- paste(f, "out", sep = "")
            cmd <- paste(shQuote(file.path(R.home("bin"), "R")),
                         "CMD BATCH --no-timing", Ropts,
                         shQuote(f), shQuote(outfile))
            cmd <- if (.Platform$OS.type == "windows") paste(cmd, "LANGUAGE=C")
            else paste("LANGUAGE=C", cmd)
           res <- system(cmd)
            if (res) {
                file.rename(outfile, paste(outfile, "fail", sep="."))
                return(invisible(1L))
            }
            savefile <- paste(outfile, "save", sep = "." )
            if (file.exists(savefile)) {
                message("  comparing ", sQuote(outfile), " to ",
                        sQuote(savefile), " ...", appendLF = FALSE)
                res <- tools::Rdiff(outfile, savefile)
                if (!res) message(" OK")
            }
        }
        setwd(owd)
    }

    invisible(0L)
}

## run all the tests in a directory: for use by R CMD check.
## trackObjs has .Rin files

.createExdotR <-
    function(pkg, pkgdir, silent = FALSE, use_gct = FALSE, addTiming = FALSE)
{
    Rfile <- paste(pkg, "-Ex.R", sep = "")
    ## might be zipped:
    exdir <- file.path(pkgdir, "R-ex")

    db <- tools:::Rd_db(basename(pkgdir), lib.loc = dirname(pkgdir))
    if (!length(db)) {
        message("no parsed files found")
        return(invisible(NULL))
    }
    if (!silent) message("  Extracting from parsed Rd's ",
                         appendLF = FALSE, domain = NA)
    files <- names(db)
    if (pkg == "grDevices")
        files <- files[!grepl("/unix|windows/", files)]
    filedir <- tempfile()
    dir.create(filedir)
    on.exit(unlink(filedir, recursive = TRUE))
    cnt <- 0L
    for(f in files) {
        nm <- sub("\\.[Rr]d$", "", basename(f))
        tools:::Rd2ex(db[[f]],
              file.path(filedir, paste(nm, "R", sep = ".")),
              defines = NULL)
        cnt <- cnt + 1L
        if(!silent && cnt %% 10L == 0L)
            message(".", appendLF = FALSE, domain = NA)
    }
    if (!silent) message()
    nof <- length(Sys.glob(file.path(filedir, "*.R")))
    if(!nof) return(invisible(NULL))

    tools:::massageExamples(pkg, filedir, Rfile, use_gct, addTiming)
    invisible(Rfile)
}


