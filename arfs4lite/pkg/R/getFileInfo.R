`getFileInfo` <-
function(filename) {
	## getFileInfo sets file information (file path, location, extension, gzippedness and endianness)
	##  also changes to .hdr if a .img is called (and checks if img/hdr pairs exist)
	## input is a filename (fullpath + name)
	## outputs an object of class nifti.header
	## calls readHeader to check nifti/analyze values	
	
	#check if file exists
	if(!file.exists(filename)) stop(paste('File',filename,'does noet exist!'))
	
	#create new instance of class fileinfo
	fileinf=new('nifti.fileinfo')
	
	#split filename into path, name and extensions
	fsplit=strsplit(tolower(filename),'\\.')[[1]]
	fname=strsplit(fsplit[1],'/')[[1]]	
	
	#set filename and path
	.nifti.fileinfo.filename(fileinf) <- fname[length(fname)]
	.nifti.fileinfo.fullpath(fileinf) <- gsub(.nifti.fileinfo.filename(fileinf),'',fsplit[1])
	
	#check gzippedness and set extension
	if(fsplit[length(fsplit)]=='gz') {
		.nifti.fileinfo.gzipped(fileinf) <- TRUE
		.nifti.fileinfo.extension(fileinf) <- fsplit[length(fsplit)-1]	
	} else {
		.nifti.fileinfo.gzipped(fileinf) <- FALSE
		.nifti.fileinfo.extension(fileinf) <- fsplit[length(fsplit)]
	}
	
	#check valid files if img/hdr pair and if IMG try and open header	
	if(.nifti.fileinfo.extension(fileinf)=='img') {
		if(.nifti.fileinfo.gzipped(fileinf)==TRUE) {
			if(file.exists(paste(.nifti.fileinfo.fullpath(fileinf),.nifti.fileinfo.filename(fileinf),'.hdr.gz',sep=''))) {
				.nifti.fileinfo.extension(fileinf) <- 'hdr'	
			} else {
				stop('No valid img/hdr pair found. HDR does not exist.\n')
			}
		} else {
			if(file.exists(paste(.nifti.fileinfo.fullpath(fileinf),.nifti.fileinfo.filename(fileinf),'.hdr',sep=''))) {
				.nifti.fileinfo.extension(fileinf) <- 'hdr'	
			} else {
				stop('No valid img/hdr pair found. HDR does not exist.\n')
			}
		} 
	}
	
	if(.nifti.fileinfo.extension(fileinf)=='hdr') {
		if(.nifti.fileinfo.gzipped(fileinf)==TRUE) {
			if(!file.exists(paste(.nifti.fileinfo.fullpath(fileinf),.nifti.fileinfo.filename(fileinf),'.img.gz',sep=''))) stop('No valid img/hdr pair found. IMG does not exist.\n')	
		} else {
			if(!file.exists(paste(.nifti.fileinfo.fullpath(fileinf),.nifti.fileinfo.filename(fileinf),'.img',sep=''))) stop('No valid img/hdr pair found. IMG does noet exist.\n')	
		}
	}
	
	#check nifti header. Returns fileinf of class nifti.header
	headinfo=readHeader(fileinf)
	
	#check if header size is 348 (if not try other endian)
	if(.nifti.header.sizeof_hdr(headinfo)!=348) {
		
		#change endian
		if(.nifti.fileinfo.endian(fileinf)=='big') .nifti.fileinfo.endian(fileinf) <- 'little' else .nifti.fileinfo.endian(fileinf) <- 'big'
		
		#check header again
		headinfo=readHeader(fileinf)
		
		if(.nifti.header.sizeof_hdr(headinfo)!=348) {
			#header is of incorrect size
			stop('Header is not of size 348 (even after swapping endian). File might not be of the correct format.\n')
		}
	}
	
	#set filetype according to header info (not based on extension)
	if(.nifti.header.magic(headinfo)=='n+1\0') {
		.nifti.header.filetype(headinfo) <- 'nifti+1'
	} else	if(.nifti.header.magic(headinfo)=='ni1\0') {
		.nifti.header.filetype(headinfo) <- 'nifti1'
		
	} else if(.nifti.header.magic(headinfo)=='\0\0\0\0') {
		.nifti.header.filetype(headinfo) <- 'analyze'
		
	} else {
		stop('Magicstring contains unknown characters. File might not be the correct format.\n')
	}
	
	#Returns object of class nifti.header		
	return(invisible(headinfo))
	
}

