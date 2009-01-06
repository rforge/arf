`writeData` <-
function(headinf,datavec) {
	## writeData writes both Header and Data info to a file
	## input is headerinfo and datavector
	## output is logical
	
	# set correct extension
	if(.nifti.header.extension(headinf)=='hdr') extension='img' else extension <- .nifti.header.extension(headinf)
	
	#open files based on gzippedness 
	if(.nifti.header.gzipped(headinf)==TRUE) {
		fn <- paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,'.gz',sep='')
		con <- gzfile(fn,open='wb')	
	} else {
		fn <-paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,sep='')
		con <- file(fn,open='wb')	
	}
	
	# for nii files write header and data in one connection
	if(extension=='nii') {
		con <- writeHeaderPart(con,headinf)
		con <- writeDataPart(con,headinf,datavec)
	
	}
	
	# for img/hdr pairs write header to .hdr and data to .img
	if(extension=='img') {
		writeHeader(headinf)
		con <- writeDataPart(con,headinf,datavec)
	}
	
	#close connection
	close(con)
	
	return(TRUE)
	
}

