#############################################
# arf3DS4 S4 COORDINATE FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#resize nifti images 
crop.volume <- function(filename,resizeToDim) {
	
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


read.FSL.mat <- function(filename) 
{
	#set separator
	sp <- .Platform$file.sep
	
	#check if only file else pre-append working directory
	if(length(grep(sp,filename))==0) filename <- paste(getwd(),sp,filename,sep='')	
	
	#read in matrix
	mat = as.matrix(read.table(filename,header=F))
	
	return(invisible(mat))
}

setRegFiles <- function(registration,examp2high='example_func2highres.mat',high2stand='highres2standard.mat',example_func='example_func.nii.gz',highres='highres.nii.gz',standard='standard.nii.gz') 
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
	
	save(registration,file=paste(.registration.fullpath(registration),.Platform$file.sep,.registration.filename(registration),sep=''))
	
	return(invisible(registration))
}

setRegParams <- function(registration) 
{
	examp = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.example(registration),sep=''))
	highres = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.highres(registration),sep=''))
	standard = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.standard(registration),sep=''))
	
	.registration.Dex(registration) = diag(c(.fmri.data.pixdim(examp)[2:4],1))
	.registration.Dhi(registration) = diag(c(.fmri.data.pixdim(highres)[2:4],1))
	.registration.Dst(registration) = diag(c(.fmri.data.pixdim(standard)[2:4],1))
	
	.registration.SXhi(registration) = diag(c(-1,1,1,1))
	.registration.SXhi(registration)[1,4] = .fmri.data.dims(highres)[2]-1
	
	.registration.Aex2hi(registration) = read.FSL.mat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2high(registration),sep=''))
	.registration.Ahi2st(registration) = read.FSL.mat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.high2stand(registration),sep=''))
	
	.registration.OXst(registration) = rbind(.fmri.data.srow_x(standard),.fmri.data.srow_y(standard),.fmri.data.srow_z(standard),c(0,0,0,1))	
	
	save(registration,file=paste(.registration.fullpath(registration),.Platform$file.sep,.registration.filename(registration),sep=''))
	return(invisible(registration))
	
}


arfToMNI <- function(xyz_coor,registration) 
{
		
	xyz = c(xyz_coor,1)
			
	examp_mm = .registration.Dex(registration)%*%xyz
	high_mm = .registration.Aex2hi(registration)%*%examp_mm
	high_vox = solve(.registration.Dhi(registration))%*%high_mm
	high_vox_flipped = .registration.SXhi(registration)%*%high_vox
	high_mm_flipped = .registration.Dhi(registration)%*%high_vox_flipped
	stand_mm_flipped = .registration.Ahi2st(registration)%*%high_mm_flipped
	stand_vox_flipped = solve(.registration.Dst(registration))%*%stand_mm_flipped
	stand_mni_flipped = .registration.OXst(registration)%*%stand_vox_flipped
	
	return(stand_mni_flipped[-length(stand_mni_flipped)])
		
}



