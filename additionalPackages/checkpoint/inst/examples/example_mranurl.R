mranUrl()  

\dontrun{
  
# Store the existing options
old_opts <- getOption("checkpoint.mranUrl")

# Set MRAN URL to different http address
options(checkpoint.mranUrl = "https://foobah")

# Set MRAN URL to local file address
options(checkpoint.mranUrl = "file:///~")

# Reset the original options
options(checkpoint.mranUrl = old_opts)
}
