# Function Library to create/determine/set data class objects
# Wouter D. Weeda, University of Amsterdam
###############################################################################



checkFiles <- function(arfdat) {
	## checkFiles is used by makeDataClass to check ik number of files and dimensions match and sets nifti read in parameters
	## other checks can be made (like on data_type etc.)
	## input is dataclass object
	## output is object class data
		
	#check if directory is valid and that there is at least one file in the directory 
	if(length(.data.datafiles(arfdat))<1 | length(.data.weightfiles(arfdat))<1)
		stop('Directory does not exist or no files in directory!')
	
	#check if number of files matches
	if(length(.data.datafiles(arfdat))!=length(.data.weightfiles(arfdat)))
		stop('Number of data and weight files do not match!')
	
	#check if dimensions of all files matches (first file in data dir is reference file)
	filenames <- c(.data.datafiles(arfdat),.data.weightfiles(arfdat))
	
	headinforef <- getFileInfo(filenames[1])
	
	if(length(filenames)>1) {
		for(i in 2:length(filenames)) {
			headinfo <- getFileInfo(filenames[i])
			if(!identical(.nifti.header.dims(headinforef),.nifti.header.dims(headinfo)))
				stop('Dimensions of file', .data.filename(headinfo),'do not match with reference file',.data.filename(headinforef),'!')
		}
	}	
	
	#return object of class data
	return(invisible(arfdat))
	
}

makeDataClass <- function(path) {
	## makeDataClass creates a dataclass object based on the directory in which the files are.
	## input is a full path to the directory of a subject-condition (which must contain /data and /weight directories)
	## output is an object of arf data class 
	
	#create new data object
	newdat <- new('data')	
	
	#add a separator to the end of the path if not already there
	sep <- .Platform$file.sep
	path <- gsub(paste(sep,sep,sep=''),sep,paste(path,sep,sep=''))
	.data.fullpath(newdat) <- path
	
		
	#set name of data class object
	name <- strsplit(path,sep)
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



makeDirStruct <- function(rootdir='',studyname='undefined_experiment',numsubs=1,subdirname='subject',condpersub=1,conddirname='condition',showstructure=T) {
	## makeDirStruct creates a directory structure based on the number of subjects and conditions per subject
	## directorynames are customizable (but cannot contain hdr/dat/img/nii/gz in the name)
	## returns the rootname of the experiment
	
	#check if rootdir is given or use current wd
	if(rootdir=='') {rootdir=getwd();warning('No rootdir specified, using working directory as root.')} 
	
	#check if experiment structure already exists
	if(file.exists(paste(rootdir,.Platform$file.sep,studyname,sep=''))) stop('Directory already exists!')
	
	#create experiment rootdir
	dir.create(paste(rootdir,.Platform$file.sep,studyname,sep=''))
	if(showstructure) cat(paste('/',studyname,'\n',sep=''))
	
	#fill in the subjects -> conditions
	for(i in 1:numsubs) {
	
		dir.create(paste(rootdir,.Platform$file.sep,studyname,.Platform$file.sep,subdirname,i,sep=''))
		if(showstructure) cat(paste('  /',subdirname,i,'\n',sep=''))
		
		for(j in 1:condpersub) {
			dir.create(paste(rootdir,.Platform$file.sep,studyname,.Platform$file.sep,subdirname,i,.Platform$file.sep,conddirname,j,sep=''))
			if(showstructure) cat(paste('    /',conddirname,j,'\n',sep=''))
			dir.create(paste(rootdir,.Platform$file.sep,studyname,.Platform$file.sep,subdirname,i,.Platform$file.sep,conddirname,j,.Platform$file.sep,'data',sep=''))
			if(showstructure) cat(paste('      /data\n',sep=''))
			dir.create(paste(rootdir,.Platform$file.sep,studyname,.Platform$file.sep,subdirname,i,.Platform$file.sep,conddirname,j,.Platform$file.sep,'weights',sep=''))
			if(showstructure) cat(paste('      /weights\n',sep=''))
		}
	}
	
	cat('\n')
	
	#return the rootpath of the experiment
	return(invisible(paste(rootdir,.Platform$file.sep,studyname,sep='')))
	
} 
