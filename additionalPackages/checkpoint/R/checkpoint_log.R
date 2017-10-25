checkpoint_log <- function(log, snapshotDate, pkg, file = NULL){
  extract_bytes <- function(log){
    ptn <- "(Content type .* length )(\\d+).*"
    gsub(ptn, "\\2", log[grep(ptn, log)])
  }
  
  extract_pkgs <- function(log){
    ptn <- "(also installing the dependencies )(.*).*"
    z <- gsub(ptn, "\\2", log[grep(ptn, log)])
    z <- iconv(z, to = "ASCII", sub = "") # strip quotin
    if(length(z) == 0) return(z)
    strsplit(z, ", ")[[1]]
  }
  if(length(log) == 0) log <- "Mocking Content type 'application/zip' length 0 bytes (0 KB)"
  
  z <- data.frame(
    timestamp = Sys.time(),
    snapshotDate = snapshotDate,
    pkg = c(extract_pkgs(log), pkg),
    bytes = extract_bytes(log),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  if(is.null(file)){
    z
  } else {
    if(!file.exists(file)){
      suppressWarnings(
        write.table(z, file = file, 
                    append = FALSE, 
                    sep = ",", 
                    row.names = FALSE, 
                    col.names = TRUE) 
      )
    } else {
      suppressWarnings(
        write.table(z, file = file, 
                    append = TRUE, 
                    sep = ",", 
                    row.names = FALSE, 
                    col.names = FALSE) 
      )
    }
  }
}  