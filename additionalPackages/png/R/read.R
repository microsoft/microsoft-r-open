readPNG <- function(source, native=FALSE, info=FALSE)
  if (info) { ## extra processing to interpret R.metadata
    if (!is.raw(source)) source <- path.expand(source)
    x <- .Call(read_png, source, native, TRUE)
    txt <- attr(x, "info")$text
    if ("R.metadata" %in% names(txt)) {
      attr(x, "metadata") <- unserialize(charToRaw(txt["R.metadata"]))
      attr(x, "info")$text <- txt[-which(names(txt) == "R.metadata")]
    }
    x
  } else .Call(read_png, if (is.raw(source)) source else path.expand(source), native, FALSE)
