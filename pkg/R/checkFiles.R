`checkFiles` <-
function(arfdat) {
	## checkFiles is used by makeDataClass to check ik number of files and dimensions match and sets nifti read in parameters
	## other checks can be made (lik on data_type etc.)
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

