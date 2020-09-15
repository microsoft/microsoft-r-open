\dontrun{

# Create temporary project and set working directory

example_project <- paste0("~/checkpoint_example_project_", Sys.Date())

dir.create(example_project, recursive = TRUE)
oldwd <- setwd(example_project)


# Write dummy code file to project

cat("library(MASS)", "library(foreach)",
    sep="\n",
    file="checkpoint_example_code.R")


# Create a checkpoint by specifying a snapshot date

library(checkpoint)
checkpoint("2014-09-17")

# Check that CRAN mirror is set to MRAN snapshot
getOption("repos")

# Check that library path is set to ~/.checkpoint
.libPaths()

# Check which packages are installed in checkpoint library
installed.packages()

# cleanup
unlink(example_project, recursive = TRUE)
setwd(oldwd)

}

