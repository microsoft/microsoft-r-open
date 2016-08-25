
## Help-related functions for use with the IDE

displayUrl <- function(url, ...){
	if (commandArgs()[1]=="" && interactive()) {
		revoIpe:::showHelp(url)
	} else {
	    browseURL(url, ...)
	}
}

isXP <- function()
{
	length(grep("XP", utils::win.version())) > 0
}

"print.help_files_with_topic" <- function (x, ...) 
{
    browser <- getOption("browser")
    topic <- attr(x, "topic")
    type <- attr(x, "type")
	if (type == "html" && commandArgs()[1]=="" && interactive() && isXP()) {
        warning("HTML help output is not supported from the R Console on Windows XP")
        type <- "text"
    }
    if (.Platform$GUI == "AQUA" && type == "html") 
        browser <- get("aqua.browser", envir = as.environment("tools:RGUI"))
    paths <- as.character(x)
    if (!length(paths)) {
        writeLines(c(gettextf("No documentation for %s in specified packages and libraries:", 
            sQuote(topic)), gettextf("you could try %s", sQuote(paste0("??", 
            topic)))))
        return(invisible(x))
    }
    port <- if (type == "html") 
        tools::startDynamicHelp(NA)
    else NULL
    if (attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic %s is not in any loaded package but can be found in the following packages:", 
            sQuote(topic))
        if (type == "html" && port > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste0("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n", 
                "<html><head><title>R: help</title>\n", "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\"UTF-8\">\n", 
                "<link rel=\"stylesheet\" type=\"text/css\" href=\"/doc/html/R.css\">\n", 
                "</head><body>\n\n<hr>\n")
            out <- c(out, "<p>", msg, "</p><br>")
            out <- c(out, "<table width=\"100%\" summary=\"R Package list\">\n", 
                "<tr align=\"left\" valign=\"top\">\n", "<td width=\"25%\">Package</td><td>Library</td></tr>\n")
            pkgs <- basename(paths)
            links <- paste0("<a href=\"http://127.0.0.1:", port, 
                "/library/", pkgs, "/help/", topic, "\">", pkgs, 
                "</a>")
            out <- c(out, paste0("<tr align=\"left\" valign=\"top\">\n", 
                "<td>", links, "</td><td>", dirname(paths), "</td></tr>\n"))
            out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
            writeLines(out, file.path(path, "all.available.html"))
			outUrl <- paste0("http://127.0.0.1:", port, "/doc/html/all.available.html")
			displayUrl(outUrl, browser)
        }
        else {
            writeLines(c(strwrap(msg), "", paste(" ", formatDL(c(gettext("Package"), 
                basename(paths)), c(gettext("Library"), dirname(paths)), 
                indent = 22))))
        }
    }
    else {
        if (length(paths) > 1L) {
            if (type == "html" && port > 0L) {
			    outUrl <- paste0("http://127.0.0.1:", port, "/library/NULL/help/", 
                  URLencode(topic, reserved = TRUE))
                displayUrl(outUrl, browser)
                return(invisible(x))
            }
            file <- paths[1L]
            p <- paths
            msg <- gettextf("Help on topic %s was found in the following packages:", 
                sQuote(topic))
            paths <- dirname(dirname(paths))
            txt <- formatDL(c("Package", basename(paths)), c("Library", 
                dirname(paths)), indent = 22L)
            writeLines(c(strwrap(msg), "", paste(" ", txt), ""))
            if (interactive()) {
                fp <- file.path(paths, "Meta", "Rd.rds")
                tp <- basename(p)
                titles <- tp
                if (type == "html" || type == "latex") 
                  tp <- tools::file_path_sans_ext(tp)
                for (i in seq_along(fp)) {
                  tmp <- try(readRDS(fp[i]))
                  titles[i] <- if (inherits(tmp, "try-error")) 
                    "unknown title"
                  else tmp[tools::file_path_sans_ext(tmp$File) == 
                    tp[i], "Title"]
                }
                txt <- paste0(titles, " {", basename(paths), 
                  "}")
                res <- menu(txt, title = gettext("Choose one"), 
                  graphics = getOption("menu.graphics"))
                if (res > 0) 
                  file <- p[res]
            }
            else {
                writeLines(gettext("\nUsing the first match ..."))
            }
        }
        else file <- paths
        if (type == "html") {
            if (port > 0L) {
                path <- dirname(file)
                dirpath <- dirname(path)
                pkgname <- basename(dirpath)
                outUrl <- paste0("http://127.0.0.1:", port, "/library/", 
                  pkgname, "/html/", basename(file), ".html")
				displayUrl(outUrl, browser)
            }
            else {
                warning("HTML help is unavailable", call. = FALSE)
                att <- attributes(x)
                xx <- sub("/html/([^/]*)\\.html$", "/help/\\1", 
                  x)
                attributes(xx) <- att
                attr(xx, "type") <- "text"
                print(xx)
            }
        }
        else if (type == "text") {
            pkgname <- basename(dirname(dirname(file)))
            temp <- tools::Rd2txt(utils:::.getHelpFile(file), out = tempfile("Rtxt"), 
                package = pkgname)
            file.show(temp, title = gettextf("R Help on %s", 
                sQuote(topic)), delete.file = TRUE)
        }
        else if (type %in% "pdf") {
            path <- dirname(file)
            dirpath <- dirname(path)
            texinputs <- file.path(dirpath, "help", "figures")
            tf2 <- tempfile("Rlatex")
            tools::Rd2latex(utils:::.getHelpFile(file), out = tf2)
            utils:::.show_help_on_topic_offline(tf2, topic, type, texinputs)
            unlink(tf2)
        }
    }
    invisible(x)
}

"index.search" <- function (topic, paths, firstOnly = FALSE) 
{
    res <- character()
    for (p in paths) {
        if (file.exists(f <- file.path(p, "help", "aliases.rds"))) 
            al <- readRDS(f)
        else if (file.exists(f <- file.path(p, "help", "AnIndex"))) {
            foo <- scan(f, what = list(a = "", b = ""), sep = "\t", 
                quote = "", na.strings = "", quiet = TRUE)
            al <- structure(foo$b, names = foo$a)
        }
        else next
        f <- al[topic]
        if (is.na(f)) 
            next
        res <- c(res, file.path(p, "help", f))
        if (firstOnly) 
            break
    }
    res
}


"?" <- function (e1, e2) 
{
    if (missing(e2)) {
        type <- NULL
        topicExpr <- substitute(e1)
    }
    else {
        type <- substitute(e1)
        topicExpr <- substitute(e2)
    }
    search <- (is.call(topicExpr) && topicExpr[[1L]] == "?")
    if (search) {
        topicExpr <- topicExpr[[2L]]
        if (is.call(te <- topicExpr) && te[[1L]] == "?" && is.call(te <- topicExpr[[2L]]) && 
            te[[1L]] == "?") {
            cat("Contacting Delphi...")
            flush.console()
            Sys.sleep(2 + rpois(1, 2))
            cat("the oracle is unavailable.\nWe apologize for any inconvenience.\n")
            return(invisible())
        }
    }
    if (is.call(topicExpr) && (topicExpr[[1L]] == "::" || topicExpr[[1L]] == 
        ":::")) {
        package <- as.character(topicExpr[[2L]])
        topicExpr <- topicExpr[[3L]]
    }
    else package <- NULL
    if (search) {
        if (is.null(type)) 
            return(eval(substitute(help.search(TOPIC, package = PACKAGE), 
                list(TOPIC = as.character(topicExpr), PACKAGE = package))))
        else return(eval(substitute(help.search(TOPIC, fields = FIELD, 
            package = PACKAGE), list(TOPIC = as.character(topicExpr), 
            FIELD = as.character(type), PACKAGE = package))))
    }
    else {
        if (is.null(type)) {
            if (is.call(topicExpr)) 
                return(utils:::.helpForCall(topicExpr, parent.frame()))
            topic <- if (is.name(topicExpr)) 
                as.character(topicExpr)
            else e1
            return(eval(substitute(help(TOPIC, package = PACKAGE), 
                list(TOPIC = topic, PACKAGE = package))))
        }
        else {
            type <- if (is.name(type)) 
                as.character(type)
            else e1
            topic <- if (is.name(topicExpr)) 
                as.character(topicExpr)
            else {
                if (is.call(topicExpr) && identical(type, "method")) 
                  return(utils:::.helpForCall(topicExpr, parent.frame(), 
                    FALSE))
                e2
            }
            if (type == "package") 
                package <- topic
            doHelp <- RevoMods:::.tryHelp(topicName(type, topic), package = package)
            if (inherits(doHelp, "try-error")) {
                if (is.language(topicExpr)) 
                  topicExpr <- deparse(topicExpr)
                stop(gettextf("no documentation of type %s and topic %s (or error in processing help)", 
                  sQuote(type), sQuote(topicExpr)), domain = NA)
            }
			doHelp
        }
    }
}

.tryHelp <- function (topic, package = NULL) 
{
    opts <- options(error = function() TRUE, show.error.messages = FALSE)
    on.exit(options(opts))
    x <- try(do.call("help", list(topic, package = package)))
    if (!inherits(x, "try-error") && length(x)) 
        RevoMods:::print.help_files_with_topic(x)
    else try(stop())
}
