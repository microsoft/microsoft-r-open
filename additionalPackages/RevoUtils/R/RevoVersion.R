"makeRevoVersion" <- function(){
	haveRevoScaleR <- !identical(system.file("DESCRIPTION", package="RevoScaleR") , "")
    if (haveRevoScaleR) {
	    if (!RevoUtils:::isMicrosoftRClient()){
			RevoEdition <- "Microsoft R Server"
		} else {
			RevoEdition <- "Microsoft R Client"
		}
    } else {
        RevoEdition <- "Microsoft R Open"
    }
	if (haveRevoScaleR){
		RSR.version <- as.character(utils::packageVersion("RevoScaleR"))
		RSR.version.components <- strsplit(RSR.version, "\\.")[[1]]
	}
    Revo.version <- version
	if (haveRevoScaleR) {
		Revo.version$major <- RSR.version.components[1]
		Revo.version$minor <- paste(RSR.version.components[2], RSR.version.components[3], sep=".")
	}
	if (haveRevoScaleR){
	    buildDate <- strsplit(utils::packageDescription("RevoScaleR", field="Built"),"; ")[[1]][3]
		buildDate2 <- strsplit(strsplit(buildDate, " ")[[1]][1], "-")[[1]]
		Revo.version$year <- buildDate2[1]
		Revo.version$month <- buildDate2[2]
		Revo.version$day <- buildDate2[3]
	} else {
		buildDate <- strsplit(utils::packageDescription("base", field="Built"),"; ")[[1]][3]
	}
	Revo.version$"svn rev" <- NULL
	Revo.version$nickname <- NULL
	if (identical(RevoEdition, "Microsoft R Server")){
		versionNum <- RSR.version
	} else if (identical(RevoEdition, "Microsoft R Client")) {
		versionNum <- as.character(utils::packageVersion("MicrosoftR"))
	} else {
		versionNum <- paste(R.version$major, R.version$minor, sep=".")
	}
	Revo.version$version.string <- paste(RevoEdition, " version " , versionNum, " (", buildDate, ")", sep="")
    Revo.version
}


