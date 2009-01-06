`writeHeader` <-
function(headinf) {
	##writeHeader writes header info to a .hdr file
	## input is headerinfo
	## output is logical
	
	## set correct extension
	if(.nifti.header.extension(headinf)=='img') extension='hdr' else extension <- .nifti.header.extension(headinf)
	
	## open based on gzippedness
	if(.nifti.header.gzipped(headinf)==TRUE) {
		fn <- paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,'.gz',sep='')
		con <- gzfile(fn,open='wb')	
	} else {
		fn <-paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,sep='')
		con <- file(fn,open='wb')	
	}
		
	## write to .hdr file
	if(extension=='hdr') con <- writeHeaderPart(con,headinf) else warning('Header info can only be written to .hdr files! No header info is written!')

	close(con)
	
	return(TRUE)
	
	
	
}

