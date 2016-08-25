## Our version of the R.home function for Revo-specific use

Revo.home <- function(component="home")
{
	if (!component %in% c("home", "licenses"))
		stop("Unknown component.")
	revoh <- system.file(package="MicrosoftR")
	switch(component, home = revoh, file.path(revoh, component))
}

