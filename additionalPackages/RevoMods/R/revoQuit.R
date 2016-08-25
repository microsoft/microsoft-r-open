## Our version of the quit() and q() functions for use in the IDE
quit<-function(save = "default", status = 0, runLast = TRUE)
{
	if (commandArgs()[1]=="" && interactive()) # if we are running the IDE
	{
		if (save != "default" || status != 0 || runLast != TRUE)
		{
			warning("Running in the Revolution IDE - shutdown parameters will be ignored.  Use the interactive dialog to save changes to your environment and solution.")
		}
		revoIpe:::closeApplication()
		Sys.sleep(5) # This is here to keep additional R code from running before the app has a chance to close
	}
	else
	{
		base::quit(save, status, runLast)
	}
}

q<-quit