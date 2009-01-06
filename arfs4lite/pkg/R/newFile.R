`newFile` <-
function(filename,templateHDR) {
	##newFile creates a new file of the given filename (and directories it should be in if they not exist)
	## input is a filename (full) and template headerinfo
	## output is headerInfo of the newly created file
	
	#split filename into path, name and extensions
	fsplit=strsplit(tolower(filename),'\\.')[[1]]
	fname=strsplit(fsplit[1],'/')[[1]]	
	
	#set filename and path
	.nifti.header.filename(templateHDR) <- fname[length(fname)]
	.nifti.header.fullpath(templateHDR) <- gsub(.nifti.header.filename(templateHDR),'',fsplit[1])
	
	#check gzippedness and set extension
	if(fsplit[length(fsplit)]=='gz') {
		.nifti.header.gzipped(templateHDR) <- TRUE
		.nifti.header.extension(templateHDR) <- fsplit[length(fsplit)-1]	
	} else {
		.nifti.header.gzipped(templateHDR) <- FALSE
		.nifti.header.extension(templateHDR) <- fsplit[length(fsplit)]
	}
	
	#create dirs if necessary
	if(!file.exists(.nifti.header.fullpath(templateHDR))) dir.create(.nifti.header.fullpath(templateHDR))
	
	# warn if file already exists
	if(file.exists(filename)) warning(paste('File',.nifti.header.filename(templateHDR),'already exists, overwriting.'))
	
	# if nii, create one file
	if(.nifti.header.extension(templateHDR)=='nii') {
		file.create(filename)
	}
	
	# if img/hdr create two files
	if(.nifti.header.extension(templateHDR)=='img' | .nifti.header.extension(templateHDR)=='hdr') {
		if(.nifti.header.gzipped(templateHDR)==TRUE) {
			file.create(paste(.nifti.header.fullpath(templateHDR),.nifti.header.filename(templateHDR),'.hdr.gz',sep='')) 
			file.create(paste(.nifti.header.fullpath(templateHDR),.nifti.header.filename(templateHDR),'.img.gz',sep='')) 
		}
		if(.nifti.header.gzipped(templateHDR)==FALSE) {
			file.create(paste(.nifti.header.fullpath(templateHDR),.nifti.header.filename(templateHDR),'.hdr',sep='')) 
			file.create(paste(.nifti.header.fullpath(templateHDR),.nifti.header.filename(templateHDR),'.img',sep='')) 
		}
	}
	
	# return header info object for created file
	return(templateHDR)
}

