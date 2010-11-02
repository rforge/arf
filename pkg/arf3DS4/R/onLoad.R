### onload

.onLoad <- function(libname,pkgname) 
{
	#create arfinternal environment
	.arfInternal = new.env(hash=T,parent=emptyenv())
	assign('.arfInternal',.arfInternal,envir=.GlobalEnv)
	
}


