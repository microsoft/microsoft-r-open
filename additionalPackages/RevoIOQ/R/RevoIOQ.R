###
# Revolution R Enterprise Installation and Operational Qualification Functionality
###

###
# Exported Functions
###

"RevoIOQ" <- function(printText=TRUE, printHTML=TRUE, 
   outdir=if (file.access(getwd(), mode=2)) file.path(tempdir(),"RevoIOQ") else file.path(getwd(),"RevoIOQ"), 
   basename=paste("Revo-IOQ-Report", format(Sys.time(), "%m-%d-%Y-%H-%M-%S"), sep="-"),
   view=TRUE, clean=TRUE, runTestFileInOwnProcess=TRUE, testLocal=FALSE, testScaleR=TRUE)
{    
    if (!require("RUnit", quietly=TRUE))
    {
        stop("RUnit package must be loaded for IOQ testing")
    }
    if (!require("tools", quietly=TRUE))
    {
        stop("The tools package must be loaded for IOQ testing")
    }
        
    # create output directory if it does not exist
    dir.create(outdir, showWarnings=FALSE)

    # ensure that the output directory is writable and throw an error if not
    if (file.access(outdir, mode=2))
    {
      stop(outdir, " is not a writable directory. Please ensure that the 'outdir' argument of RevoIOQ() defines a path to a writable directory.")
    }

    # store current working directory
    curwd <- getwd()

    # use output directory as working directory; reset on exit
    setwd(outdir)
    on.exit(setwd(curwd))

    # save copy of .GlobalEnv workspace to temporary workspace
    tempdb <- "RUnitGlobalEnvCopy"
    RevoIOQ:::copyWorkspace(".GlobalEnv", target.name=tempdb)
    
    # define the Revo RUnit test suite and run tests
    testDirRoot <- system.file("unitTests", package="RevoIOQ")
    testDirs <- unlist(lapply(file.path(testDirRoot,c("R","Revo")), function(x, os) file.path(x, c("common",os)), os=.Platform$OS.type))  
    if (testScaleR) {	
		if (!identical(system.file("DESCRIPTION", package="RevoScaleR") , "")) 
		{
			 # include the RevoScaleR tests if they exist
			 raTestDirRoot <- system.file("unitTests", package="RevoScaleR")
			 raTestDirs <-   file.path(raTestDirRoot, c("common", .Platform$OS.type))
			 testDirs <- c(testDirs, raTestDirs)
		}
	}
    if (testLocal) 
    {
         # Create test files for local packages
	 testPath <- file.path(outdir, "localTests")
         dir.create(testPath, showWarnings=FALSE)
         pkgsAll <- as.data.frame(installed.packages()[,c("Version", "Priority")])
         pkgsBase <- row.names(subset(pkgsAll, Priority=="base"))
         pkgsRecommended <- row.names(subset(pkgsAll, Priority=="recommended"))
         pkgsRevo <- row.names(pkgInfo)
         pkgsLocal <- setdiff(row.names(pkgsAll), c(pkgsBase, pkgsRecommended, pkgsRevo))
         for (i in pkgsLocal) 
         {
              testFile <- paste("runit.local.", i, ".R", sep="")
              writeLocalUnitTest(pkg=i, file=testFile, outdir=testPath)
	 }
	 testDirs <- c(testDirs, testPath)
    }
    testSuiteName <- Revo.version$version.string
    if (is.null(testSuiteName)) {
		testSuiteName <- tempfile()
	}	
    testSuite <- defineTestSuite(name=testSuiteName, dirs=testDirs)
    testResult <- runRevoTestSuite(testSuite, runTestFileInOwnProcess=runTestFileInOwnProcess)
    
    # create base filename 
    filename <- file.path(outdir, basename)
   
    ## print text report
    if (printText)
    {
        textfile <- paste(filename,".txt", sep="")
        printTextProtocol(testResult, fileName=textfile, showDetails=TRUE)
        
        if (view && !printHTML)
        {
            file.show(textfile)            
        }
    }
    
    # print HTML report
    if (printHTML)
    {
        htmlfile <- paste(filename,".html", sep="")
        printHTMLProtocol(testResult, fileName=htmlfile)
        
        if (view)
        {
            ## Would like this to be a try/catch loop
            ## Current behavior of browseURL doesn't behave correctly
            if (!(!is.na(grep("linux",R.version["os"])[1]) && !capabilities()["X11"])) {
                browseURL(file.path("file://", htmlfile))
            }
            else if (file.exists(textfile)) {
                    file.show(textfile)
                }
            else cat("Report written to ", htmlfile, " \n")
                     
         }
    }

    # cleanup
    if (clean)
    {
        graphics.off()
        allfiles <- list.files()
        cleanup <- union(grep(".ps", allfiles), grep(".pdf", allfiles))
        file.remove(allfiles[cleanup])
    }

    # restore .GlobalEnv workspace
    RevoIOQ:::restoreWorkspace(target.name=tempdb)

    # obtain summary of tests
    status <- getErrors(testResult)
    
    # return test summary
    #
    # nFail : Contains the number of RUnit tests that failed. For example, checkTrue(FALSE) counts as a failure.
    #         Only one failure per test function is recorded as the remaining checks are skipped.
    #
    # nErr  : Contains the number of errors thrown during the testing. For example, log("a") counts as an error
    #         while checkException(log("a")) and try(log("a") do NOT count as errors.
    
    invisible(status$nFail == 0 && status$nErr == 0)
}

###
# Hidden Functions
###

"isComputeNode" <- function()
{
    isComputeNode <- FALSE
	if (.Platform$OS.type == "windows" && length(grep("Enterprise", Revo.version$version.string)) && 
	       !file.exists(file.path(Revo.home(), "IDE32"))) {
		isComputeNode <- TRUE
    }
    isComputeNode
}

"saveRUnitSession" <- function(opt=options(), packages=NULL, datasets=NULL, envir=.GlobalEnv)
{
    ## saveRUnitSession : saves the current session data objects, options, loaded packages, etc. that are NOT specified
    ##                    by the list of input arguments to this function. Later, the saveRUnitSession function attempts
    ##                    to restore those objects define in this list.
    ##
    ## state       : list. saved session information as output by RevoIOQ:::saveRUnitSessionRUnitSession().
    ##
    ## VALUE       : list of current options, loaded packages, data objects, and environment used to collect object information.

    if (!is.null(packages) && is(packages,"character"))
    {   
        packages <- packages[!packages %in% .packages()]
    }
    
    if (!is.null(datasets) && is(datasets,"character"))
    {
        datasets <- datasets[!datasets %in% objects(envir=envir)]
    }
    
    list(options=opt, packages=packages, datasets=datasets, envir=envir)
}

"restoreRUnitSession" <- function(state=RevoIOQ:::saveRUnitSessionRUnitSession())
{
    ## restoreRUnitSession : restores the session objects, options, loaded packages, etc. from a previously saved session.
    ##
    ## state       : list. saved session information as output by RevoIOQ:::saveRUnitSessionRUnitSession().
    ##
    ## VALUE       : invisible(TRUE).
 
    if (is.null(state))
    {
        warning("Empty state")
        return(FALSE)        
    }
    
    if (!is.list(state))
    {
        warning("State is not a list")
        return(FALSE)
    }
    
    if (!all(names(state) %in% c("datasets","packages","options","envir")))
    {
        warning("State must be a list with names \"datasets\", \"packages\", \"envir\", and \"options\"")
        return(FALSE)
    }
    
    # restore options
    if (!is.null(state$options))
    {
        res <- try(options(state$options))
        checkTrue(!is(res, "try-error"), msg="options restoration failed")        
    }
    
    # unload packages if not previously loaded
    for (pkgname in state$packages)
    {
        pkg <- paste("package", pkgname, sep=":")
        if (pkgname %in% .packages())
        {
          dum <- eval(parse(text=paste("try(detach(", pkg, "))")))
        }
        
        # do not throw an error here if the package that we are attempting to detach
        # is already detached, which would otherwise result in an RUnit failure
        #checkTrue(!is(res, "try-error"), msg=paste("Detaching", pkg, "failed"))    
    }
    
    # unload data sets if not previously loaded
    for (dataset in state$datasets)
    {
        res <- eval(parse(text=paste("try(remove(", dataset, ", envir=state$envir))")))
        checkTrue(!is(res, "try-error"), msg=paste("Detaching", dataset, "data set failed"))    
    }
    
    invisible(TRUE)
}

"copyWorkspace" <- function(source.name, target.name="tempdb", target.pos=2, 
        all.names=FALSE, append=FALSE, pattern)
{
    ## copyWorkspace : copy workspace on search path to new workspace
    ##
    ## source.name : character string, name of workspace to copy, e.g., ".GlobalEnv" or "package:foo"
    ## target.name : character string, name of workspace to create and store copies of source objects, Default: "tempdb"
    ## target.pos  : integer, the position to place the new target workspace in on the search() list
    ## all.names   : logical, passed directly to ls() function in gathering objects to copy
    ## pattern     : character string, passed directly to ls() function in gathering objects to copy
    ## append      : logical, if FALSE and target.name workspace already exists then that workspace is
    ##               first detached the re-created to create a new workspace. If TRUE and target.name
    ##               workspace already exists then that workspace is left in tact and further populated
    ##               with the objects copied from the source wokspace
    ##
    ## SIDEFX      : a hidden ".workspace" object is also placed in the target.name workspace and
    ##               contains information needed for restoration of the original source workspace
    ##               via the restoreWorkspace() function
    ##
    ## VALUE       : NULL is returned invisibly
    
    if (!is(source.name,"character") || !is(target.name,"character"))
    {
        stop("source.name and target.name must be an object of class character")
    }   
    
    # detach target. db if requested
    if (target.name %in% search() && !append)
    {
        eval(parse(text=paste("detach(", target.name, ", pos=", target.pos, ")")))
    }
    
    # create target. db in search list
    if (!target.name %in% search())
    {
        attach(NULL, pos=target.pos, name=target.name)
    }
    
    # obtain list of objects in specified environment
    objs <- ls(source.name, all.names=all.names, pattern=pattern)
    
    # copy the source objects into the target environment
    for (i in objs)
    {
        assign(i, value=get(i, pos=source.name), pos=target.name)
    }
    
    # assign hidden file to copied workspace containing restoration information
    assign(".workspace", value=list(source.name=source.name, target.name=target.name, all.names=all.names), 
            pos=target.pos)
    
    invisible(NULL)	
}

"restoreWorkspace" <- function(target.name="tempdb", append=TRUE, detach.copy=TRUE)
{
    ## restoreWorkspace : restore workspace from that created by copyWorkspace() function. 
    ##
    ## target.name : character string, name of workspace that was created by copyWorkspace 
    ##               to store copies of source objects. Default: "tempdb".
    ## append      : logical argument sent directly to copyWorkspace function in restoring the source workspace.
    ##               Setting this argument to FALSE will result in the original workspace being detached
    ##               and recreated prior to being repopulated. If TRUE, restored objects with unique
    ##               names will be added to original workspace while those with existing names will be
    ##               overwritten with the vlaues originall copied via the copyWorkspace() function. 
    ##               Default: TRUE.
    ## detach.copy : logical, if TRUE the target.name workspace is detached after the original source
    ##               workspace has been restored. Default: TRUE.
    ##
    ## VALUE       : NULL is returned invisibly

    # obtain restoraton information
    x <- get(".workspace", pos=target.name)

    # copy workspace to target
    RevoIOQ:::copyWorkspace(source.name=x$target.name, target.name=x$source.name, 
            target.pos=x$source.name, all.names=x$all.names, append=append)
    
    if (detach.copy)
    {
        eval(parse(text=paste("detach(", x$target.name, ")")))
    }
    
    invisible(NULL)
}

"testPackageLoadability" <- function(pkg, lib.loc=NULL, quiet=TRUE)
{
    ## testPackageLoadability : test whether a package can be loaded or not
    ##
    ## pkg         : character string. name of the package to test.
    ## lib.loc     : character string. path to the library where the package is installed.
    ## quiet       : logical. passed to the quiet argument of the require() function. Default: TRUE.
    ##
    ## VALUE       : logical. If TRUE, the packag is loadable.

    (length(find.package(pkg, lib.loc=lib.loc, quiet=quiet)) > 0) && do.call("require", list(package=pkg, lib.loc=lib.loc, quiet=quiet))
}


"testPackageVersion" <- function(pkg, ver, lib.loc=NULL)
{
   ## testPackageVersion : test whether a specifed version of a package is installed and exists on .libPaths()
   ##
   ## pkg         : character string. name of the package to test.
   ## ver         : character string. defines the version of the installed.
   ## lib.loc     : vector of character strings. defines the library paths to search for specified package. If NULL,
   ##               .libPaths() is used.
   ##
   ## VALUE       : logical. If TRUE, the specified version of the package is installed into one of the paths in .libPaths().

   # find all occurrences of the pkg on the specified library path(s)
   libs <- if (is.null(lib.loc)) .libPaths() else lib.loc
   pkglibs <- unlist(lapply(libs, function(x, pkg) find.package(package=pkg, lib.loc=x, quiet=TRUE), pkg=pkg))
   if (!length(pkglibs)) return(FALSE)
   
   pkgvers <- unlist(lapply(pkglibs, function(x, pkg) installed.packages(lib.loc=dirname(x))[pkg,"Version"], pkg=pkg))
   any(pkgvers %in% ver)
}
 
"writeLocalUnitTest" <- function(
  pkg,
  file=paste("runit.", pkg, ".localTest.R", sep=""),
  outdir=getwd(),
  quiet=TRUE)
{
   ## Function to generate simple RUnit test for additional packages found on system
     # define wrapper RUnit test functions
   "checkPackageLoadability" <- function(pkg, quiet)
   {
      RevoIOQ:::testPackageLoadability(pkg = pkg, quiet = quiet)
   }
   
   "checkPackageExamples" <- function (pkg, outDir = file.path(tempdir(), "runPackageExamplesDir"), types = c("examples", "tests", "vignettes")) 
   {
      if (!(file.exists(outDir) && file.info(outDir)$isdir))
      {
        dir.create(outDir, showWarnings=FALSE, recursive=TRUE)
        on.exit(unlink(outDir))
      }
      
      tools::testInstalledPackage(pkg = pkg, outDir = outDir, types = types) == 0L
   }
   # define local functions
   "RUnitTest" <- function(x, pkg, type)
     paste("\n\"test.", pkg, ".", type, "\" <- function() ", x, sep="") 

   "testString" <- function(FUN, ...)
   {
      wrapFunction <- function(x, FOO) gsub("[ ]+"," ", paste(FOO, "(", x, ")", sep=""))
      wrapFunction(paste(deparse(as.call(list(name=FUN, ...))), collapse=""), "checkTrue")
   }
   "openFile" <- function(path)
   {        
      if (file.exists(path))
        unlink(path)        
      file(path, "wt")
   }

   "writeHeader" <- function(FID)
   {
     if (!is.character(FID)) FID <- deparse(substitute(FID))
     
	 # file header
     text <- paste("cat(paste(\"###\\n# local package RUnit test functions \\n###\\n\\n\"), file=", FID, ")", sep="")
     eval(parse(text=text))

     text <- paste("cat(\"\\\"checkPackageLoadability\\\" <- \", deparse(checkPackageLoadability, width=80), sep=\"\\n\", file=", FID, ")", sep="")
     eval(parse(text=text))

 	 text <- paste("cat(\"\\\"checkPackageExamples\\\" <- \", deparse(checkPackageExamples, width=80), sep=\"\\n\", file=", FID, ")", sep="")
     eval(parse(text=text))

     invisible(NULL)
   }
   
   "writeTest" <- function(FUN, ARGS, DESC, FID)
   {
     if (!is.character(FUN)) FUN <- deparse(substitute(FUN))
     if (!is.character(FID)) FID <- deparse(substitute(FID))
     text <- paste("cat(RUnitTest(testString(\"", FUN, "\", ", ARGS, "), pkg, \"", DESC, "\"), file=", FID, ")", sep="")
     eval(parse(text=text))
     invisible(NULL)
   }


   fid <- openFile(file.path(outdir, file))
   writeHeader(fid)
   writeTest(checkPackageLoadability,paste("pkg=\"", pkg, "\", quiet=quiet", sep=""), "loadability",fid)
   writeTest(checkPackageExamples, paste("pkg=\"", pkg, "\"", sep=""), "examples", fid)
   close(fid)
}

runRevoTestSuite <- function(testSuites, useOwnErrorHandler=TRUE, runTestFileInOwnProcess=FALSE) {
  ##@bdescr
  ## This is the main function of the runit framework, modified so that each test file
  ## can be run in a separate process. It finds all the relevant
  ## test files and triggers all the required actions. At the end it creates a test
  ## protocol data object. 
  ## IMPORTANT to note, the random number generator is (re-)set to the default
  ## methods specifed in defineTestSuite() before each new test case file is sourced. 
  ## This guarantees that each new test case set defined together in one file can rely
  ## on the default, even if the random number generator version is being reconfigured 
  ## in some previous test case file(s).
  ##@edescr
  ##
  ##@in  testSuites         : [list] list of test suite lists
  ##@in  useOwnErrorHandler : [logical] TRUE (default) : use the runit error handler
  ##@ret                    : [list] 'RUnitTestData' S3 class object
  ##
  ##@codestatus : testing
  
  ##  preconditions
  if (!is.logical(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' has to be of type logical.")
  }
  if (length(useOwnErrorHandler) != 1) {
    stop("argument 'useOwnErrorHandler' has to be of length 1.")
  }
  if (is.na(useOwnErrorHandler)) {
    stop("argument 'useOwnErrorHandler' may not contain NA.")
  }
  
  
  ##  record RNGkind and reinstantiate on exit
  rngDefault <- RNGkind()
  on.exit(RNGkind(kind=rngDefault[1], normal.kind=rngDefault[2]))
  
  oldErrorHandler <- getOption("error")
  ## reinstall error handler
  on.exit(options(error=oldErrorHandler), add=TRUE)
  
  ## initialize TestLogger
  assign(".testLogger", RUnit:::.newTestLogger(useOwnErrorHandler), envir = .GlobalEnv)

  ## main loop
  if(isValidTestSuite(testSuites)) {
    testSuites <- list(testSuites)
 }
  for (i in seq_along(testSuites)) {
    testSuite <- testSuites[[i]]
    if(!isValidTestSuite(testSuite)) {
      errMsg <- paste("Invalid test suite",testSuite$name,". Test run aborted.")
      stop(errMsg)
    }
    .testLogger$setCurrentTestSuite(testSuite)
    testFiles <- list.files(testSuite$dirs,
                            pattern = testSuite$testFileRegexp,
                            full.names=TRUE)
    for(testFile in testFiles) {
	if (runTestFileInOwnProcess) {
        cat("Running test file ", testFile, "\n")
        storefile <- gsub("\\\\", "/", tempfile(pattern="RevoIOQ"))
        save(.testLogger, testFile, testSuite, file=storefile)
        testscript <- gsub("\\\\", "/", tempfile(pattern="RevoIOQTest"))
        cat("library(RevoIOQ)\n",
            if (length(grep("RevoScaleR", testFile)) > 0) "require(RevoScaleR)\n",		
			"Sys.setlocale('LC_COLLATE', 'C')\n",
            paste("load('", storefile, "', envir=.GlobalEnv)\n", sep=""),
			"RUnitEnv <- .GlobalEnv\n",
            "RNGkind(kind=testSuite$rngKind, normal.kind=testSuite$rngNormalKind)\n",
             paste("RUnit:::.sourceTestFile('", testFile,"', '", testSuite$testFuncRegexp, "')\n",sep=""),
		 paste("save(.testLogger, file='", storefile, "')\n",sep=""), "q('no')\n",
             file=testscript)
        Rbinpath <- file.path(R.home(), "bin")
        if (.Platform$OS.type=="windows" && compareVersion(as.character(getRversion()), "2.12.0") >= 0){
		if (.Machine$sizeof.pointer==4){
			Rbinpath <- file.path(Rbinpath, "i386")
		} else {
			Rbinpath <- file.path(Rbinpath, "x64")
		}
        }
        Rscriptpath <- file.path(Rbinpath, "Rscript")
        rstatus <- system(paste(Rscriptpath, " --default-packages=", " ",  testscript, sep=""))
		if (rstatus != 0) {
			.testLogger$setCurrentSourceFile(testFile)
			.testLogger$addError(testFuncName = testFile, 
				errorMsg=paste("R exited abnormally while running test file", testFile))
			save(.testLogger, file=storefile)
		}
        load(storefile, envir=.GlobalEnv)
        unlink(c(storefile, testscript))
	} else {
        ## set a standard random number generator.
        RNGkind(kind=testSuite$rngKind, normal.kind=testSuite$rngNormalKind)
		## set collation so locale settings won't affect sort
		Sys.setlocale("LC_COLLATE", "C")
        
        RUnit:::.sourceTestFile(testFile, testSuite$testFuncRegexp)
      }
    }
  }

  ret <- .testLogger$getTestData()
  
  return(ret)
}

