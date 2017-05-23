readNews <- function(package)
{
	if (missing(package)) {
        if (!isRRE())
        {
            rver <-  paste(c("#", "r", version$major, unlist(strsplit(version$minor, "\\."))), collapse="")
            rNewsUrl <- paste("http://mran.revolutionanalytics.com/news/", rver, sep="")
            browseURL(rNewsUrl)
        } else {
            if ((.Platform$OS.type=="windows" ) ||
            !(!is.na(grep("linux", R.version["os"])[1]) && 
                    !capabilities()["X11"])){	
                RShowDoc("NEWS")
            } else {
                file.show(file.path(R.home("doc"), "NEWS"))
            }
        }
 		return(invisible(NULL))
	}
	if (file.exists(system.file("NEWS", package=package))){
		file.show(system.file("NEWS", package=package))
		return(invisible(NULL))
		}
	if (!file.exists(system.file("NEWS.Rd", package=package))) {
		stop("No NEWS.Rd or NEWS file in specified package")
	} else {
	    if ((.Platform$OS.type=="windows" ) ||
		!(!is.na(grep("linux", R.version["os"])[1]) && 
                !capabilities()["X11"])){	
			HTMLout <- paste(tempfile(), ".html", sep="")
			tools:::Rd2HTML(system.file("NEWS.Rd", package=package), out=HTMLout)
			browseURL(HTMLout)
			queueForDelete(HTMLout)
		} else {
			txtOut <- paste(tempfile(), ".txt", sep="")
			tools:::Rd2txt(system.file("NEWS.Rd", package=package), out=txtOut)
			file.show(txtOut)
			unlink(txtOut)
		}
	}
	invisible(NULL)
}

queueForDelete <- function(file)
{
	if (!is.character(file)) 
		stop("file argument must be a character vector")
	if (exists(".revoDeleteList", where=1, inherits=FALSE )) {
		r <- get(".revoDeleteList", pos=1)
		if (!class(r) == "character" || !length(r)) 
			stop("invalid deletion list")
		r <- c(r, file)
		file <- r
	}
	assign(".revoDeleteList", file, pos=1)	
	invisible(file)
}
	
unlinkDeleteList <- function()
{
	if (exists(".revoDeleteList", where=1, inherits=FALSE )) {
		unlink(.revoDeleteList)
		rm(.revoDeleteList)
	}
	invisible(NULL)
}
		