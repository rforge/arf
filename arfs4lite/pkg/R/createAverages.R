`createAverages` <-
function(arfdat,refheaddat=NULL,refheadW=NULL) {
	## createAverages averages the data and weightfiles 
	## input is an object of Data class
	## output is logical (datafiles are saved in avg directory)
	
	# check if avg directory exists
	if(!file.exists(.data.fullpath(arfdat))) dir.create(.data.fullpath(arfdat))
	
	# add trial data to avgdat and weightdat
	avgdat <- avgweight <- 0
	for(i in 1:.data.trials(arfdat)) {
		avgdat <- avgdat + .fmri.data.datavec(readData(.data.datafiles(arfdat)[i]))
		avgweight <- avgweight + .fmri.data.datavec(readData(.data.weightfiles(arfdat)[i]))
	}
	
	# divide avgdat by trialnumber and avgweight by trialnumber^2
	avgdat <- avgdat / .data.trials(arfdat)
	avgweight <- avgweight / .data.trials(arfdat)^2

	# get header info of first file (or reference file id supplied)
	if(is.null(refheaddat)) headinf <- readHeader(getFileInfo(.data.datafiles(arfdat)[1])) else headinf <- readHeader(getFileInfo(refheaddat))
	
	# write header and datainfo for datafiles
	filename <- paste(.data.fullpath(arfdat),'avg/avgdata.',.nifti.header.extension(headinf),sep='')
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.data.avgdatfile(arfdat) <- filename
	writeData(headinf,avgdat)
	
	# get header info of first file (or reference file id supplied)
	if(is.null(refheadW)) headinf <- readHeader(getFileInfo(.data.weightfiles(arfdat)[1])) else headinf <- readHeader(getFileInfo(refheadW))
	
	# write header and datainfo for weightfiles
	filename <- paste(.data.fullpath(arfdat),'avg/avgweight.',.nifti.header.extension(headinf),sep='')
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.data.avgWfile(arfdat) <- filename	
	writeData(headinf,avgweight)
	
	# return data class object	
	return(invisible(arfdat))
	
}

