"test.MRO.snapshot.date" <- function()
{
      mro.expected.snapshot.date <- utils::packageDescription("RevoUtils")$MRANDate
      reposinfo <- getOption("repos")
      # Extract snapshot date from repository information.
      slashpos <- gregexpr("/", reposinfo, fixed = TRUE)[[1]]
      slashpos.len <- length(slashpos)
      lastslash.pos <- slashpos[slashpos.len]
      mro.verified.snapshot.date <- substring(reposinfo, lastslash.pos + 1, nchar(reposinfo))
      mro.verified.snapshot.date <- mro.verified.snapshot.date[1]
      names(mro.verified.snapshot.date) <- NULL
      checkEquals(mro.expected.snapshot.date, mro.verified.snapshot.date)
}

"test.MRO.MKL.installed" <- function()
{   
      DEACTIVATED("Test temporarily deactivated.")
      
      if (Sys.info()['sysname'] == "Darwin")
      {
          DEACTIVATED("Test deactivated because MKL does not get installed on Mac OS systems.")
      }

      Blas.Linux.basename <- "libRblas.so"
      Blas.Win.basename <- "RevoUtilsMath.dll"
      Rblas.Linux.path <- file.path(R.home(), "lib", Blas.Linux.basename, sep = "/")
      Rblas.Linux.path <- substring(Rblas.Linux.path, 1, 50)
      Rblas.Windows.path <- paste(R.home(), "library/RevoUtilsMath/libs/x64", Blas.Win.basename, sep = "/")
      
      if(Sys.info()['sysname'] == "Linux")
      {        
	     on.Ubuntu <- system("find /usr -iname '*lsb_release*'", intern = TRUE)
         if (length(on.Ubuntu) > 0)
         {
             blas.test.expected.value <- "0"
             ifelse(file.exists(Rblas.Linux.path), is.blas.installed <- "0", is.blas.installed <- "1")
             test.result <- checkEquals(blas.test.expected.value, is.blas.installed)
         } else {
             # Get a list of symbols contained in 'libRblas.so'.
             system(paste("nm", Rblas.Linux.path, ">", "mklinfo.txt", sep = " "))
             # Now search for the symbol 'i_free' in mklinfo - it should be there if MKL is installed and return "0".
             blas.test.expected.value <- "0"    
             is.blas.installed <- system("grep -q 'i_free' mklinfo.txt && echo $?", intern=TRUE) 
             test.result <- checkEquals(blas.test.expected.value, is.blas.installed)
         }           
         if (test.result) {
             cat("Test passed - MKL Blas libraries.\n")
         } else {   
             cat("Test failed - missing MKL Blas libraries from MRS install. \n")
         }
      }
      
      if(Sys.info()['sysname'] == "Windows")
      {
            # On Windows just check for existance of 'RevoUtilsMath.dll' on the build system,
            # since file is only there if MKL components are installed.
            blas.test.expected.value <- TRUE
            is.blas.installed <- file.exists(Rblas.Windows.path)
            test.result <- checkEquals(blas.test.expected.value, is.blas.installed)
            if (test.result)
            {
                cat("Test passed - MKL Blas libraries installed.\n")
            } else {
                cat("Test failed - missing MKL Blas libraries from MRS install.\n")      
            }
      }
}	

"test.MRO.default.packages.install" <- function()
{
    DEACTIVATED("Test temporarily deactivated.")

    MRO.libpath <- .libPaths()[1]
    ms.rpackages.list <- c("checkpoint", "curl", "deployrRserve", "doParallel", "foreach",
          "iterators", "jsonlite", "MicrosoftML", "MicrosoftR", "mrsdeploy", "png", "R6", "RevoIOQ", "RevoMods", 
          "RevoUtils", "RevoUtilsMath", "RUnit")
    
    MRO.R.BaseObjects.Dir <- file.path(MRO.libpath, "RevoIOQ/unitTestData/capvecdata")
    base.rpackages.list <- row.names(installed.packages(priority = "high"))
    expected.package.list <- sort(c(base.rpackages.list, ms.rpackages.list))
    load(file.path(MRO.R.BaseObjects.Dir, "MRO.pack.sav"))
    test.result <- checkEquals(expected.package.list, installed.package.list)
    if (test.result)
    {
        cat("Test passed - all expected R packages installed.\n")
    }
}
	
"test.MRO.capabilities" <- function()
{
    DEACTIVATED("Test temporarily deactivated.")
    
    MRO.libpath <- .libPaths()[1]
    sysname <- Sys.info()['sysname']
    sysname <- sysname[[1]]
    MRO.R.BaseObjects.Dir <- file.path(MRO.libpath, "RevoIOQ/unitTestData/capvecdata")   
    
    if(sysname == "Linux")
    {        			  
	    if(file.exists("/etc/redhat-release") == TRUE) {
            load(file.path(MRO.R.BaseObjects.Dir, "capvec-CentOS7.sav"))
        } else if (file.exists("/etc/lsb-release") == TRUE)
        {    
            load(file.path(MRO.R.BaseObjects.Dir, "capvec-MRO-UB1604.sav"))
        } else if (file.exists("/etc/SuSE-release") == TRUE)
        {
            load(file.path(MRO.R.BaseObjects.Dir, "capvec-SLES11.sav"))
        }
    }
        
    if(sysname == "Windows")
    {    
		load(file.path(MRO.R.BaseObjects.Dir, "capvec-MRO-Win10.sav"))
    }

    if (sysname == "Darwin")
    {
        load(file.path(MRO.R.BaseObjects.Dir, "capvec-MRO-Mac.sav"))   
    }

    curbuild.capabilities.list <- capabilities()   
    # After loading base object, this will create vector 'cap.vec'.
	checkEquals(cap.vec, curbuild.capabilities.list)
}       	