# checkpoint test common functions
cleanCheckpointFolder <- function(snapshotDate, checkpointLocation) {
  folder = checkpointPath(snapshotDate, checkpointLocation, type = "snapshot")
  unlink(folder, recursive = TRUE, force = TRUE)}
