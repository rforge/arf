`makeDataClass` <-
function(path) {
	## makeDataClass creates a dataclass object based on the directory in which the files are.
	## input is a full path to the directory of a subject-condition (which must contain /data and /weight directories)
	## output is an object of arf data class 
	
	#create new data object
	newdat <- new('data')	
	
	#add an / to the end of the path if not already there
	path <- gsub('//','/',paste(path,'/',sep=''))
	
	.data.fullpath(newdat) <- path
	
		
	#set name of data class object
	name <- strsplit(path,'/')
	.data.name(newdat) <- as.character(name[[1]][length(name[[1]])])
		
	#set lists of fullpaths to the datafiles (excluding .hdr files)
	nameslist <-  tolower(list.files(paste(path,'data',sep=''),full.names=T))
	whichhdr <- grep('.hdr',nameslist,value=F)
	if(length(whichhdr)>1) 
		.data.datafiles(newdat) <- nameslist[-whichhdr]
	else
		.data.datafiles(newdat) <- nameslist
	
	#set lists of fullpaths to the weightfiles (excluding .hdr files)
	nameslist <-  tolower(list.files(paste(path,'weights',sep=''),full.names=T))
	whichhdr <- grep('.hdr',nameslist,value=F)
	if(length(whichhdr)>1)
		.data.weightfiles(newdat) <- nameslist[-whichhdr]
	else
		.data.weightfiles(newdat) <- nameslist
		
	#check validity (valid pairs, valid type and matching dims) returns nifti.header for reference file (first file in dir)
	newdat <- checkFiles(newdat)
	
	#set number of trials to length datanames
	.data.trials(newdat) <- length(.data.datafiles(newdat))
	
	#return object of class 'Data'		
	return(invisible(newdat))
	
}

