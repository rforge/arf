`readData` <-
function(filename) {
	## readData reads in nifti/analyze datafiles
	## input is a filename to read from
	## output is vector of data
	
	#obtain header info
	headinf <- readHeader(getFileInfo(filename))
	
	## set correct extension
	if(.nifti.header.extension(headinf)=='hdr') extension='img' else extension <- .nifti.header.extension(headinf)
	
	#open based on gzippedness
	if(.nifti.header.gzipped(headinf)==TRUE) {
		con <- gzfile(paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,'.gz',sep=''),open='rb')	
	} else {
		con <- file(paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',extension,sep=''),open='rb')		
	}
	
	if(con) {
		#create new vecdat object
		data <- new('fmri.data',headinf)
		
		#set length of data to read in
		n=(.nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4]*.nifti.header.dims(headinf)[5]*(.nifti.header.bitpix(headinf)/8))
		
		#read everthing before vox_offset
		readBin(con,raw(),.nifti.header.vox_offset(headinf))
		
		#read in data
		.fmri.data.datavec(data) <- readBin(con, what=.nifti.header.data.type(headinf), n=n, size=(.nifti.header.bitpix(headinf)/8), signed=.nifti.header.data.signed(headinf), endian=.nifti.header.endian(headinf))
		
		#close connections
		closeAllConnections()
		
		#return datavector
		return(invisible(data))
		
	} else stop('Unable to open connection') 	
}

