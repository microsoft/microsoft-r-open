.onAttach <- function(...) {
  msg <- paste(
    "",
    "checkpoint: Part of the Reproducible R Toolkit from Microsoft",
    "https://mran.microsoft.com/documents/rro/reproducibility/",
    sep="\n")
  packageStartupMessage(msg)
}

