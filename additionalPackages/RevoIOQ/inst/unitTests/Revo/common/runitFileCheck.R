test.install.filecheck <- function(checkNew = FALSE, checkPython = TRUE) {
    DEACTIVATED("Test temporarily deactivated.")
	Install_Root_RHOME <- normalizePath(R.home())
    is.MRS <- isTRUE(!identical(system.file(package = "RevoScaleR"), ""))
    is.RClient <- RevoUtils:::isMicrosoftRClient()
    BaseLineFileDir <- "MR-BaseLineFiles"
    current_OS_type <- Sys.info()['sysname'][[1]]
    if (current_OS_type == "Darwin") {
        ## The normalizePath() function changes the default path on Mac-OSX, so don't normalize it.
        Install_Root_RHOME <- "/Library/Frameworks/R.framework"
    }
    Python_HOME_dir <- "C:/Program Files/Microsoft/R Server/PYTHON_SERVER"  
    python.dirvalue <- file.access(Python_HOME_dir)
    names(python.dirvalue) <- NULL
    python.testval <- identical(as.numeric(python.dirvalue), 0)
    if (python.testval && !isTRUE(is.RClient)) {
        expected_MRS_files_dir <- file.path(Install_Root_RHOME, "library/RevoIOQ/unitTestData", BaseLineFileDir, sep = "/")
        installed_MRS_files.list <- list.files(path = Install_Root_RHOME, all.files = TRUE, full.names = TRUE,
                                               recursive = TRUE, include.dirs = FALSE)
        installed_MRS_files.list <- unlist(lapply(installed_MRS_files.list, normalizePath))
        installed_MRS_python_files.list <- list.files(path = Python_HOME_dir, all.files = TRUE, full.names = TRUE,
                                                     recursive = TRUE, include.dirs = FALSE)
        installed_MRS_python_files.list <- unlist(lapply(installed_MRS_python_files.list, normalizePath))
        if (current_OS_type == "Windows") {
            BaseLineFile <- "MRSInstallerWinBaseLine.txt"
            PythonBaseLineFile <- "MRSInstallerWin-Python-BaseLine.txt"
        }
        if (current_OS_type == "Linux") {
            BaseLineFile <- "MRSInstallerLinuxBaseLine.txt"
            PythonBaseLineFile <- "MRSInstallerLinux-Python-BaseLine.txt"
        }
        expected_MRS_files.list <- readLines(paste(expected_MRS_files_dir, BaseLineFile, sep = ""), skipNul = TRUE)
        expected_MRS_python_files.list <- readLines(paste(expected_MRS_files_dir, PythonBaseLineFile, sep = ""), skipNul = TRUE)

        # The next five lines needed to get a proper list of baseline files, after intially reading of file above.
        # Only necessary if running on Windows.
        if (current_OS_type == "Windows") {
            numBaseFiles <- length(expected_MRS_files.list)
            numBaseFiles.python <- length(expected_MRS_python_files.list)
            fileseq <- seq(1, numBaseFiles, by = 2)
            fileseq.python <- seq(1, numBaseFiles.python, by = 2)
            expected_MRS_files.list <- expected_MRS_files.list[fileseq]
            expected_MRS_python_files.list <- expected_MRS_python_files.list[fileseq.python]
            colon.pos <- gregexpr(":", expected_MRS_files.list[1])[[1]]
            colon.python.pos <- gregexpr(":", expected_MRS_python_files.list[1])[[1]]
            expected_MRS_files.list[1] <- substring(expected_MRS_files.list[1], colon.pos - 1, nchar(expected_MRS_files.list[1]))
            expected_MRS_python_files.list[1] <- substring(expected_MRS_python_files.list[1], colon.python.pos - 1, nchar(expected_MRS_python_files.list[1]))
        }

        # Construct the list of files missing from baseline and from the MRS/MRO/RClient install.
        baseline.to.installed.index <- expected_MRS_files.list %in% installed_MRS_files.list
        installed.to.baseline.index <- installed_MRS_files.list %in% expected_MRS_files.list
        files.missing.from.install <- expected_MRS_files.list[baseline.to.installed.index == FALSE]
        new.files.missing.baseline <- installed_MRS_files.list[installed.to.baseline.index == FALSE]
        baseline.to.installed.python.index <- expected_MRS_python_files.list %in% installed_MRS_python_files.list
        installed.to.baseline.python.index <- installed_MRS_python_files.list %in% expected_MRS_python_files.list
        files.missing.from.python.install <- expected_MRS_python_files.list[baseline.to.installed.python.index == FALSE]
        new.files.missing.python.baseline <- installed_MRS_python_files.list[installed.to.baseline.python.index == FALSE]
    } else if (is.MRS && !isTRUE(is.RClient)) {
        expected_MRO_files_dir <- file.path(Install_Root_RHOME, "library/RevoIOQ/unitTestData", BaseLineFileDir, sep = "/")
        installed_MRO_files.list <- list.files(path = Install_Root_RHOME, all.files = TRUE, full.names = TRUE,
                                               recursive = TRUE, include.dirs = FALSE)
        if (current_OS_type != "Darwin") {
            installed_MRO_files.list <- unlist(lapply(installed_MRO_files.list, normalizePath))
        }

        if (current_OS_type == "Windows") {
            BaseLineFile <- "MROInstallerWinBaseLine.txt"
            expected_MRO_files.list <- readLines(paste(expected_MRO_files_dir, BaseLineFile, sep = ""), skipNul = TRUE)

            # The next five lines needed to get a proper list of baseline files, after intially reading of file above.
            # Only necessary if running on Windows.
            numBaseFiles <- length(expected_MRO_files.list)
            fileseq <- seq(1, numBaseFiles, by = 2)
            expected_MRO_files.list <- expected_MRO_files.list[fileseq]
            colon.pos <- gregexpr(":", expected_MRO_files.list[1])[[1]]
            expected_MRO_files.list[1] <- substring(expected_MRO_files.list[1], colon.pos - 1, nchar(expected_MRO_files.list[1]))
        }
        if (current_OS_type == "Linux") {
            if (!isTRUE(is.RClient)) {
                BaseLineFile <- "MROInstallerLinuxBaseLine.txt"
                expected_MRO_files.list <- readLines(paste(expected_MRO_files_dir, BaseLineFile, sep = ""), skipNul = TRUE)
            }
        }
        if (current_OS_type == "Darwin") {
            BaseLineFile <- "MROInstallerMacBaseLine.txt"
            expected_MRO_files.list <- readLines(paste(expected_MRO_files_dir, BaseLineFile, sep = ""), skipNul = TRUE)
        }
        baseline.to.installed.index <- expected_MRO_files.list %in% installed_MRO_files.list
        installed.to.baseline.index <- installed_MRO_files.list %in% expected_MRO_files.list
        files.missing.from.install <- expected_MRO_files.list[baseline.to.installed.index == FALSE]
        new.files.missing.baseline <- installed_MRO_files.list[installed.to.baseline.index == FALSE]
    } else {
        if (current_OS_type == "Windows") {
            BaseLineFile <- "RClientInstallerWinBaseLine.txt"
        } else {
            BaseLineFile <- "RClientInstallerLinuxBaseLine.txt"
        }
        cat(BaseLineFile, "\n")
        expected_RClient_files_dir <- file.path(Install_Root_RHOME, "library/RevoIOQ/unitTestData", BaseLineFileDir, sep = "/")
  	    installed_RClient_files.list <- list.files(path = Install_Root_RHOME, all.files = TRUE, full.names = TRUE,
                  recursive = TRUE, include.dirs = FALSE)
	    installed_RClient_files.list <- unlist(lapply(installed_RClient_files.list, normalizePath))
        expected_RClient_files.list <- readLines(paste(expected_RClient_files_dir, BaseLineFile, sep = ""), skipNul = TRUE)
        
	   # The next five lines needed to get a proper list of baseline files, after intially reading of file above.
	   numBaseFiles <- length(expected_RClient_files.list)
	   fileseq <- seq(1, numBaseFiles, by = 2)
	   expected_RClient_files.list <- expected_RClient_files.list[fileseq]
	   colon.pos <- gregexpr(":", expected_RClient_files.list[1])[[1]]
	   expected_RClient_files.list[1] <- substring(expected_RClient_files.list[1], colon.pos-1, nchar(expected_RClient_files.list[1]))        
        
	   baseline.to.installed.index <- expected_RClient_files.list %in% installed_RClient_files.list
	   installed.to.baseline.index <- installed_RClient_files.list %in% expected_RClient_files.list
	   files.missing.from.install <- expected_RClient_files.list[baseline.to.installed.index == FALSE]
	   new.files.missing.baseline <- installed_RClient_files.list[installed.to.baseline.index == FALSE]
    }    
    if (is.MRS && current_OS_type == "Windows" && !isTRUE(is.RClient)) { 
        if (checkPython) {
			missing.baseline.len <- length(new.files.missing.baseline) + length(new.files.missing.python.baseline)
			missing.install.len <- length(files.missing.from.install) + length(files.missing.from.python.install)
			checkEquals(missing.baseline.len, 0)
			checkEquals(missing.install.len, 0)
			if ((length(new.files.missing.baseline) > 0) || (length(new.files.missing.python.baseline) > 0) && isTRUE(checkNew)) {
				cat("\n\n")
				cat("Test FAILED - differences between BaseLine file and installed files.\n")
				cat("These files are missing from the baseline file:", BaseLineFile, ".\n") 
				cat(new.files.missing.baseline, fill = TRUE)
				cat(new.files.missing.python.baseline, fill = TRUE)   
			} else if (((length(files.missing.from.install) > 0) || (length(files.missing.from.python.install) > 0) && !isTRUE(checkNew))) {
				
			    
				cat("\n\n")
				cat("Test FAILED - differences between installed files and baseline default file.\n")
				cat("These files are listed in the baseline file, but missing from the install:\n")
				cat("==========================================================================\n") 
				cat(files.missing.from.install, fill = TRUE)
				cat(files.missing.from.python.install, fill = TRUE)
			} 
			if((length(new.files.missing.baseline) == 0) && (length(new.files.missing.python.baseline) == 0) && length((files.missing.from.python.install) == 0) && (length(files.missing.from.install) == 0)) {
				cat("BaseLine files and installed files are IDENTICAL.\n")
			}
		} else {
		    missing.baseline.len <- length(new.files.missing.baseline)
			missing.install.len <- length(files.missing.from.install)
			checkEquals(missing.baseline.len, 0)
			checkEquals(missing.install.len, 0)
			if ((length(new.files.missing.baseline) > 0) && isTRUE(checkNew)) {
				cat("\n\n")
				cat("Test FAILED - differences between BaseLine file and installed files.\n")
				cat("These files are missing from the baseline file:", BaseLineFile, ".\n") 
				cat(new.files.missing.baseline, fill = TRUE)
		    } else if ((length(files.missing.from.install) > 0) && !isTRUE(checkNew)) {
			    
				cat("\n\n")
				cat("Test FAILED - differences between installed files and baseline default file.\n")
				cat("These files are listed in the baseline file, but missing from the install:\n")
				cat("==========================================================================\n") 
				cat(files.missing.from.install, fill = TRUE)
		    }
		    
		}
    } else {
        missing.baseline.len <- length(new.files.missing.baseline)
		missing.install.len <- length(files.missing.from.install)
		checkEquals(missing.baseline.len, 0)
		checkEquals(missing.install.len, 0)
		if ((length(new.files.missing.baseline) > 0) && isTRUE(checkNew)) {
            cat("\n\n")
            cat("Test FAILED - differences between BaseLine file and installed files.\n")
            cat("These files are missing from the baseline file:", BaseLineFile, ".\n")
            cat(new.files.missing.baseline, fill = TRUE)
        } else if ((length(files.missing.from.install) > 0) && !isTRUE(checkNew)) {
            cat("\n\n")
            cat("Test FAILED - differences between installed files and baseline default file.\n")
            cat("These files are listed in the baseline file, but missing from the install:\n")
            cat("==========================================================================\n")
            cat(files.missing.from.install, fill = TRUE)
        }
        if ((length(new.files.missing.baseline) == 0) && (length(files.missing.from.install) == 0)) {
            cat("BaseLine files and installed files are IDENTICAL.\n")
        }
    }
}

