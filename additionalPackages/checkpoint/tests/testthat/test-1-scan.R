if(interactive()) library(testthat)

context("scan for library, require, :: and :::")

unCheckpoint()

collapse <- function(...) paste(..., sep="\n", collapse="\n")
foo <- function(token, insert){
  collapse(sprintf(token, insert))
}

project_root <- file.path(tempdir(), "checkpoint-test-temp")
unlink(project_root, recursive = TRUE)
dir.create(project_root, recursive = TRUE, showWarnings = FALSE)

code <- collapse(
  foo("library(%s)", letters[1:2]),
  foo("require(%s)", letters[3:4]),
  foo("%s::foo()", letters[5:6]),
  foo("%s:::bar()", letters[7:8]),
  "track <- setClass('track', slots = c(x='numeric', y='numeric'))"
)

test_that("scanRepoPackages finds dependencies", {
  
  # Write dummy code file to project
  codefile <- file.path(project_root, "code.R")
  cat(code, file = codefile)
  
  found <- scanForPackages(project = project_root)
  expect_equal(found$pkgs, sort(c(letters[1:8], "methods")))
  
  file.remove(codefile)
})

test_that("finds packages in Rmd files", {
  if(!knitr.is.installed()) skip("knitr not available")
  
  # Write dummy knitr code file to project
  knit <- sprintf("```{r}\n%s\n```", code)
  knitfile <- file.path(project_root, "knit.Rmd")
  cat(knit, file = knitfile)
  
  found <- scanForPackages(project = project_root, use.knitr = TRUE)
  expect_equal(found$pkgs, c(letters[1:8], "methods"))
  file.remove(knitfile)
})

test_that("auto-installs knitr and rmarkdown", {
  if(!knitr.is.installed()) skip("knitr not available")
  
  # Write dummy knitr code file to project
  knit <- sprintf("```{r}\n%s\n```", code)
  knitfile <- file.path(project_root, "knit.Rmd")
  cat(knit, file = knitfile)
  
  found <- scanForPackages(project = project_root, use.knitr = TRUE, 
                               auto.install.knitr = TRUE)
  expect_equal(found$pkgs, c(letters[1:8], "methods", "knitr"))
  file.remove(knitfile)
})

# scanRepoPackages allows switching between knitr and Sweave
code <- collapse(
  "\\documentclass{article}",
  "\\begin{document}",
  "\\SweaveOpts{concordance=TRUE}",
  "Some text",
  "<<foo, cache = FALSE>>=",
  "  library(abc)",
  "  x <- FALSE",
  "@",
  "",
  "<<bar, eval = FALSE>>=",
  "  1+1",
  "@",
  "Nothing interesting here",
  "\\end{document}"
)
codefile <- file.path(project_root, "code.Rnw")
cat(code, file = codefile)

test_that("Sweave scans Rnw files with eval=FALSE chunks", {
  if(!knitr.is.installed()) skip("knitr not available")

  found <- scanForPackages(project = project_root, use.knitr = FALSE, 
                               scan.rnw.with.knitr = FALSE)
  expect_equal(found$pkgs, "abc")
  expect_equal(found$error, character(0))
})

# test_that("knitr scans Rnw files with eval=FALSE chunks", {
#   found <- scanForPackages(project = project_root, use.knitr = TRUE,
#                                scan.rnw.with.knitr = TRUE)
#   expect_equal(found$pkgs, "abc")
#   expect_equal(found$error, character(0))
# })

test_that("lapplyProgressBar returns the value of lapply", {
  expect_equal(lapplyProgressBar(1:5, identity), lapply(1:5, identity))
})


unlink(codefile)
unlink(project_root)
