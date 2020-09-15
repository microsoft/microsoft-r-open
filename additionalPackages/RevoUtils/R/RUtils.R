totalSystemMemory <- function()
{
    if (identical(.Platform$OS.type,"unix"))
    {   
	    if (identical(as.character(Sys.info()["sysname"]), "Darwin")){
			mem <- try(system2("sysctl", args="hw.memsize", stdout=TRUE), silent=TRUE)
			if (inherits(mem, "try-error") || !length(grep("[0-9]",mem)))
			{
				mem <- NA
			}
			else
			{		
				mem <- as.numeric(strsplit(mem, ": ")[[1]][2])		
			}
		}
		else
		{
			mem <- try(system2("head", args=c("-1", "/proc/meminfo"), stdout=TRUE), silent=TRUE)
			if (inherits(mem, "try-error") || !length(grep("[0-9]",mem)))
			{
				mem <- NA
			}
			else
			{
				mem <- as.numeric(strsplit(mem, " +")[[1]][2])
			}
		}
    }
    else if (identical(.Platform$OS.type, "windows"))
    {
        mem <- substring(system2("wmic", args=c("OS", "get", "TotalVisibleMemorySize", "/Value"), stdout=TRUE), 24)[3]
        mem <- as.numeric(mem)
    }
    else 
    {
        mem <- NA
    }
    mem
}