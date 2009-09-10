#############################################
# arf3DS4 S4 COORDINATE FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#cropVolume
#createRegs
#createFuncs
#checkRegs
#checkFuncs
#setRegFiles
#setRegParams
#arfToMNI

cropVolume <- 
function(filename,resizeToDim) 
#resize nifti images (crops last slices to destination dimensions)
{
	
	dat <- readData(filename)
	data_array <- .fmri.data.datavec(dat)
	
	x=.fmri.data.dims(dat)[2]
	y=.fmri.data.dims(dat)[3]
	z=.fmri.data.dims(dat)[4]
	
	nx=resizeToDim[2]
	ny=resizeToDim[3]
	nz=resizeToDim[4]
	
	dim(data_array) <- c(x,y,z)
	
	if(nx>x | ny>y | nz>z) stop('can only crop images!')
	if(nx==x & ny==y & nz==z) stop('no crop necessary')
	
	cat('Dims',.fmri.data.dims(dat),'>> ')
		
	if(nx<x) {
		remx <- seq(nx+1,x,1)
		data_array <- data_array[-remx,,]
		.fmri.data.dims(dat)[2]=nx
	} 
	
	if(ny<y) {
		remy <- seq(ny+1,y,1)
		data_array <- data_array[,-remy,]
		.fmri.data.dims(dat)[3]=ny
	} 
	
	if(nz<z) {
		remz <- seq(nz+1,z,1)
		data_array <- data_array[,,-remz]
		.fmri.data.dims(dat)[4]=nz
	} 
	
	cat(.fmri.data.dims(dat),'\n')
	cat('filename',.fmri.data.filename(dat),'\n')	
	cat('datavec should be',nx*ny*nz,'long and is now',length(as.vector(data_array)),'\n')
	
	if(file.exists(.fmri.data.filename(dat))) file.remove(.fmri.data.filename(dat))
	writeData(dat,as.vector(data_array))
}


read.FSL.mat <- 
function(filename)
#read FSL affine matrix file
{
	#set separator
	sp <- .Platform$file.sep
	
	#check if only file else pre-append working directory
	if(length(grep(sp,filename))==0) filename <- paste(getwd(),sp,filename,sep='')	
	
	#read in matrix
	mat = as.matrix(read.table(filename,header=F))
	
	return(invisible(mat))
}


 
createRegs <- 
function(arfdata) 
#createRegs creates registration files for each
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#make new registration object
	registration <- new('registration')
	
	#get betafiles plus path of registration
	filelist <- .data.betafiles(arfdata)
	path <- .data.regDir(arfdata)
	
	#check betafile integrity and make paths in regDir
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			#get info from betafile and set linkedfile
			info <- getFileInfo(filename)
			dirname <- .nifti.fileinfo.filename(info)
			
			#create dir and create regfilename
			if(!file.exists(paste(path,sp,dirname,sep=''))) dir.create(paste(path,sp,dirname,sep=''))
			
			.registration.linkedfile(registration) <- filename
			.registration.fullpath(registration) <- paste(path,sp,.nifti.fileinfo.filename(info),sep='')
			.registration.filename(registration) <- .data.regRda(arfdata)
			
			#save objects
			save(registration,file=paste(.registration.fullpath(registration),sp,.registration.filename(registration),sep=''))
						
		} else warning('No betafile found to match reg to')
	}
	
}


checkRegs <- 
function(arfdata,overwrite=F) 
#checkRegs checks the integrity of the registrationRda's and sets fullpath based on the experiment - data
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#allisWell
	allIsWell = TRUE
	
	#get betafiles plus path of registration
	filelist <- .data.betafiles(arfdata)
	path <- .data.regDir(arfdata)
	
	#check betafile integrity and make paths in regDir
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			#get info from betafile and set linkedfile
			info <- getFileInfo(filename)
			fn <- paste(path,sp,.nifti.fileinfo.filename(info),sp,.data.regRda(arfdata),sep='')
			
			if(file.exists(fn)) {
			
				#set correct paths
				registration = loadRda(fn)
				.registration.linkedfile(registration) <- filename
				.registration.fullpath(registration) <- paste(path,sp,.nifti.fileinfo.filename(info),sep='')
				.registration.filename(registration) <- .data.regRda(arfdata)
			
				#save objects
				save(registration,file=paste(.registration.fullpath(registration),sp,.registration.filename(registration),sep=''))
			} 
		} else {
			#warning('No betafile found to match reg to')
			allIsWell = FALSE
		}
	}
	return(allIsWell)	
}

setRegFiles <- 
function(registration,examp2high='example_func2highres.mat',high2stand='highres2standard.mat',example_func='example_func.nii.gz',highres='highres.nii.gz',standard='standard.nii.gz') 
#setRegfiles fills the registration object with the correct matrices and nifti images (uses FSL standard as default)
{
	
	#set and check regFiles
	.registration.examp2high(registration) <- examp2high
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2high(registration),sep=''))) stop('ex2high does not exist')
	.registration.high2stand(registration) <- high2stand
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.high2stand(registration),sep=''))) stop('high2stand does not exist')
	.registration.example(registration) <- example_func
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.example(registration),sep=''))) stop('standard does not exist')
	.registration.highres(registration) <- highres
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.highres(registration),sep=''))) stop('highres does not exist')
	.registration.standard(registration) <- standard
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.standard(registration),sep=''))) stop('standard does not exist')
	
	#save regfile object
	save(registration,file=paste(.registration.fullpath(registration),.Platform$file.sep,.registration.filename(registration),sep=''))
	
	#return registration object
	return(invisible(registration))
}


setRegParams <- 
function(registration) 
#setRegparams reads in registration parameters from the files in registration object and sets appropriate matrices
{
	#load registration volumes
	examp = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.example(registration),sep=''))
	highres = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.highres(registration),sep=''))
	standard = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.standard(registration),sep=''))
	
	#Set pixdim matrices
	.registration.Dex(registration) = diag(c(.fmri.data.pixdim(examp)[2:4],1))
	.registration.Dhi(registration) = diag(c(.fmri.data.pixdim(highres)[2:4],1))
	.registration.Dst(registration) = diag(c(.fmri.data.pixdim(standard)[2:4],1))
	
	#set x-axis swap matrix
	.registration.SXhi(registration) = diag(c(-1,1,1,1))
	.registration.SXhi(registration)[1,4] = .fmri.data.dims(highres)[2]-1
	
	#set affine transformation matrices
	.registration.Aex2hi(registration) = read.FSL.mat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2high(registration),sep=''))
	.registration.Ahi2st(registration) = read.FSL.mat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.high2stand(registration),sep=''))
	
	#set origin offset matrix
	.registration.OXst(registration) = rbind(.fmri.data.srow_x(standard),.fmri.data.srow_y(standard),.fmri.data.srow_z(standard),c(0,0,0,1))	
	
	#save object
	save(registration,file=paste(.registration.fullpath(registration),.Platform$file.sep,.registration.filename(registration),sep=''))
	
	return(invisible(registration))
	
}



arfToMNI <- 
#arfToMNI converts arf-native voxel locations to MNI_152 standard coordinates		
function(xyz_coor,registration) 
{
		
	xyz = c(xyz_coor,1)
	
	#examp_vox to mm
	examp_mm = .registration.Dex(registration)%*%xyz
	
	#examp_mm to high_mm
	high_mm = .registration.Aex2hi(registration)%*%examp_mm
	
	#high_mm to high_vox
	high_vox = solve(.registration.Dhi(registration))%*%high_mm
	
	#x-axis flipped
	high_vox_flipped = .registration.SXhi(registration)%*%high_vox
	
	#high_vox to high_mm
	high_mm_flipped = .registration.Dhi(registration)%*%high_vox_flipped
	
	#high_mm to standard_mm
	stand_mm_flipped = .registration.Ahi2st(registration)%*%high_mm_flipped
	
	#standard_mm to standard_vox 
	stand_vox_flipped = solve(.registration.Dst(registration))%*%stand_mm_flipped
	
	#standard_vox to MNI (origin offset)
	stand_mni_flipped = .registration.OXst(registration)%*%stand_vox_flipped
	
	#return MNI
	return(stand_mni_flipped[-length(stand_mni_flipped)])
		
}


createFuncs <- 
function(arfdata) 
#createFuncs creates registration files for each 
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#make new registration object
	functional<- new('functional')
	
	#get betafiles plus path of registration
	filelist <- .data.betafiles(arfdata)
	path <- .data.funcDir(arfdata)
	
	#check betafile integrity and make paths in regDir
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			#get info from betafile and set linkedfile
			info <- getFileInfo(filename)
			dirname <- .nifti.fileinfo.filename(info)
			
			#create dir and create regfilename
			if(!file.exists(paste(path,sp,dirname,sep=''))) dir.create(paste(path,sp,dirname,sep=''))
			
			.functional.linkedfile(functional) <- filename
			.functional.fullpath(functional) <- paste(path,sp,.nifti.fileinfo.filename(info),sep='')
			.functional.filename(functional) <- .data.funcRda(arfdata)
			
			#save objects
			save(functional,file=paste(.functional.fullpath(functional),sp,.functional.filename(functional),sep=''))
			
		} else warning('No betafile found to match functional data to')
	}
	
}

checkFuncs <- 
function(arfdata,overwrite=F) 
#checkFuncs checks the integrity of the functionalRda and sets fullpath based on the experiment - data
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#allisWell
	allIsWell = TRUE
	
	#get betafiles plus path of registration
	filelist <- .data.betafiles(arfdata)
	path <- .data.funcDir(arfdata)
	
	#check betafile integrity and make paths in regDir
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			#get info from betafile and set linkedfile
			info <- getFileInfo(filename)
			fn <- paste(path,sp,.nifti.fileinfo.filename(info),sp,.data.funcRda(arfdata),sep='')
			
			if(file.exists(fn)) {
				
				#set correct paths
				functional = loadRda(fn)
				.functional.linkedfile(functional) <- filename
				.functional.fullpath(functional) <- paste(path,sp,.nifti.fileinfo.filename(info),sep='')
				.functional.filename(functional) <- .data.funcRda(arfdata)
				
				#save objects
				save(functional,file=paste(.functional.fullpath(functional),sp,.functional.filename(functional),sep=''))
			
			} 
		} else {
			#warning('No betafile found to match functional data to')
			allIsWell = FALSE
		}
	}
	return(allIsWell)	
}


