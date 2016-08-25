## Our version of the source function for use in the IDE
revoSource<-function(file, ...)
{
	if (commandArgs()[1]=="" && interactive() && # if we are running the IDE
		is.character(file) && file != "" && # and file contains a character path
		revoIpe:::isDebugActive() == TRUE) # and we are running debuggable code
	{
		file <- normalizePath(file)
		revoIpe:::createInstrumentedSourceFile(file)		
		instFile<-paste(file, .Revo.inst.ext, sep="")
		if (!file.exists(instFile))
		{
			warning("Could not source file for debugging in the Revolution IDE. File will be sourced normally.")
		}
		else
		{
			file<-instFile
		}
	}
	base::source(file, ...)
}	