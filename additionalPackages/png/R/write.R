writePNG <- function(image, target = raw(), dpi = NULL, asp = NULL, text = NULL, metadata = NULL) {
  if (!is.null(text) && !is.character(text)) text <- sapply(text, as.character)
  if (!is.null(metadata)) {
    rmd <- rawToChar(serialize(metadata, NULL, TRUE))
    text <- if (is.null(text)) c(R.metadata=rmd) else c(text, R.metadata=rmd)
  }
  if (inherits(target, "connection")) {
    r <- .Call(write_png, image, raw(), dpi, asp, text)
    writeBin(r, target)
    invisible(NULL)
  } else invisible(.Call(write_png, image, if (is.raw(target)) target else path.expand(target), dpi, asp, text))
}
