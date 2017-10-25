# Empty date field returns current repo

oldRepos <- getOption("repos")
setSnapshot()

# Valid snapshot date
# Connects to MRAN to check for valid URL, so skip on CRAN
\dontrun{
setSnapshot("2014-11-16")
}

# Invalid snapshot date (in future), returns error
\dontrun{
setSnapshot("2100-01-01")
}

options(repos = oldRepos)
