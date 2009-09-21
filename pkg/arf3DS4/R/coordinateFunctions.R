#############################################
# arf3DS4 S4 COORDINATE FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#cropVolume
#cropVolumeAuto
#createRegs
#createFuncs
#setRegFiles
#setRegParams
#arf2MNI
#MNI2arf
#setFuncFile
#flipAxis
#euclidDist
#makeLowResStruct
#makeLowResStructAvg


cropVolume <- 
function(filename,resizeToDim,quiet=F) 
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
	
	doCrop=TRUE
	
	if(nx>x | ny>y | nz>z) {warning('can only crop images!');doCrop=FALSE}
	if(nx==x & ny==y & nz==z) {doCrop=FALSE}
	
	if(doCrop) {
		if(!quiet) cat('Dims',.fmri.data.dims(dat),'>> ')
			
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
		
		if(!quiet) cat(.fmri.data.dims(dat),'\n')
		if(!quiet) cat('filename',.fmri.data.filename(dat),'\n')	
		if(!quiet) cat('datavec should be',nx*ny*nz,'long and is now',length(as.vector(data_array)),'\n')
		cat(paste('[crop] cropped file ',.fmri.data.filename(dat),' (',x,'x',y,'x',z,') to: ',nx,'x',ny,'x',nz,sep=''),'\n')
		if(file.exists(.fmri.data.filename(dat))) file.remove(.fmri.data.filename(dat))
		writeData(dat,as.vector(data_array))
	}
}


cropVolumeAuto <- 
function(betadir,quiet=T) 
#resize nifti images (crops last slices to destination dimensions)
{
	
	
	#determinesmallest
	filelist <- list.files(betadir,full=T)
	dims=matrix(NA,length(filelist),8)
	for(i in 1:length(filelist)) {
		
		dat = readData(filelist[i])
		dims[i,]=.fmri.data.dims(dat)
		
	}
	
	minx=min(dims[,2])
	miny=min(dims[,3])
	minz=min(dims[,4])
	
	for(i in 1:length(filelist)) cropVolume(filelist[i],c(1,minx,miny,minz),quiet)
	
	
}

readFSLmat <- 
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
	#return(invisble(registration))
}


setRegFiles <- 
function(registration,examp2stand='example_func2standard.mat',examp2high='example_func2highres.mat',high2stand='highres2standard.mat',example_func='example_func.nii.gz',highres='highres.nii.gz',standard='standard.nii.gz') 
#setRegfiles fills the registration object with the correct matrices and nifti images (uses FSL standard as default)
{
	
	#set and check regFiles
	.registration.examp2stand(registration) <- examp2stand
	if(!file.exists(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2stand(registration),sep=''))) stop('ex2stand does not exist')
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
	.registration.Aex2hi(registration) = readFSLmat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2high(registration),sep=''))
	.registration.Ahi2st(registration) = readFSLmat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.high2stand(registration),sep=''))
	
	#set origin offset matrix
	.registration.OXst(registration) = rbind(.fmri.data.srow_x(standard),.fmri.data.srow_y(standard),.fmri.data.srow_z(standard),c(0,0,0,1))	
	
	#save object
	save(registration,file=paste(.registration.fullpath(registration),.Platform$file.sep,.registration.filename(registration),sep=''))
	
	return(invisible(registration))
	
}



arf2MNI <- 
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

MNI2arf <- 
#arfToMNI converts MNI_152 standard coordinates	to arf-native voxel locations	
function(xyz_coor,registration) 
{
	
	xyz = c(xyz_coor,1)
	
	#inverse standard_vox to MNI (origin offset)
	stand_mni_flipped = solve(.registration.OXst(registration))%*%xyz
	
	#inverse standard_mm to standard_vox 
	stand_vox_flipped = (.registration.Dst(registration))%*%stand_mni_flipped
	
	#inverse high_mm to standard_mm
	stand_mm_flipped = solve(.registration.Ahi2st(registration))%*%stand_vox_flipped
	
	#inverse high_vox to high_mm
	high_mm_flipped = solve(.registration.Dhi(registration))%*%stand_mm_flipped
	
	#inverse x-axis flipped
	high_vox_flipped = solve(.registration.SXhi(registration))%*%high_mm_flipped
	
	#inverse high_mm to high_vox
	high_vox = (.registration.Dhi(registration))%*%high_vox_flipped
	
	#inverse examp_mm to high_mm
	high_mm = solve(.registration.Aex2hi(registration))%*%high_vox
	
	#inverse examp_vox to mm
	examp_mm = solve(.registration.Dex(registration))%*%high_mm
			
	#return arf
	return(examp_mm[-length(examp_mm)])
	
}

MNI2atlas <- 
#MNI converts MNI_152 standard coordinates to tal index 		
function(xyz_coor,registration) 
{
	xyz = c(xyz_coor,1)
		
	#inverse standard_vox to MNI (origin offset)
	stand_mni_flipped = solve(.registration.OXst(registration))%*%xyz
			
	#return arf
	return(stand_mni_flipped [-length(stand_mni_flipped)])
}

flipAxis <-
function(data_array,axis=c('x','y','z'))
#flips the axis of a data_array
{
	axis = match.arg(axis)
	
	new_dat = data_array
	
	affine = diag(c(1,1,1,1))
	
	if(axis=='x') {
		affine[1,1] = -1
		affine[1,4] = dim(data_array)[1]+1
		for(x in 1:dim(data_array)[1]) {
			nc = affine%*%c(x,1,1,1)
			new_dat[nc[1],,] = data_array[x,,]
		}
	} 
	
	if(axis=='y') {
		affine[2,2] = -1
		affine[2,4] = dim(data_array)[2]+1
		for(y in 1:dim(data_array)[2]) {
			nc = affine%*%c(1,y,1,1)
			new_dat[,nc[2],] = data_array[,y,]
		}
	} 
	
	if(axis=='z') {
		affine[3,3] = -1
		affine[3,4] = dim(data_array)[3]+1
		for(z in 1:dim(data_array)[3]) {
			nc = affine%*%c(1,1,z,1)
			new_dat[,,nc[3]] = data_array[,,z]
		}
	} 
	
	return(new_dat)	
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
			
			.functional.linkedfiles(functional) <- filename
			.functional.filename(functional) <- paste(path,sp,dirname,sp,.data.funcRda(arfdata),sep='')
			
			#save objects
			save(functional,file=paste(.functional.filename(functional),sep=''))
			
		} else warning('No betafile found to match functional data to')
	}
	#return(invisble(functional))
}



setFuncFiles <- 
function(experiment=.experiment,func_data='filtered_func_data.nii.gz') 
###
{
	#set separator
	sp <- .Platform$file.sep
	
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		spath <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,.experiment.subject.names(experiment)[sdirs],sep='')
		
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			cpath <- paste(spath,sp,.experiment.conditionDir(experiment),sp,.experiment.condition.names(experiment)[cdirs],sp,.experiment.dataDir(experiment),sp,.experiment.funcDir(experiment),sep='')
			
			funcdirs <- list.files(cpath)
		
			for(trs in 1:length(funcdirs)) {
				
				functional <- loadRda(paste(cpath,sp,funcdirs[trs],sp,.experiment.funcRda(experiment),sep=''))
				
				if(length(functional)>0) {
					.functional.fullpath(functional) <- paste(spath,sp,.experiment.funcDir(experiment),sp,funcdirs[trs],sep='')
					.functional.functionaldata(functional) <- func_data				
				} else cat('[func] functional.Rda not found\n') 
	
				#save regfile object
				save(functional,file=paste(.functional.filename(functional),sep=''))
				
			}
			
		}
		
	}
		
	
	#return registration object
	return(invisible(functional))
}

euclidDist <-
function(arfmodel,thres=5,quiet=F) 
#calculate euclidian distances between estimates
{
	
	theta = matrix(.model.estimates(arfmodel),10)
	#distmat = matrix(0,ncol(theta),ncol(theta))
	#showmat = matrix(0,ncol(theta),ncol(theta))
	
	p=0
	for(i in 1:(ncol(theta)-1)) {
		for(j in (i+1):ncol(theta)) {
			
			dist=0
			for(k in 1:3) dist = dist + (theta[k,i]-theta[k,j])^2
			#distmat[i,j]=c(sqrt(dist))
			
			opp=FALSE
			if(theta[10,j]<0 & theta[10,i]>0) opp=TRUE
			if(theta[10,j]>0 & theta[10,i]<0) opp=TRUE
			
			if(i!=j & distmat[i,j]<thres & opp) {
				if(!quiet) cat('region',i,'[',round(theta[c(1,2,3,4,5,6,10),i]),'] -',j,'[',round(theta[c(1,2,3,4,5,6,10),j]),'] distance:',sqrt(dist),'\n')
				#showmat[i,j]=1/sqrt(dist)
			}
			p=p+1	
		}
	}
	#return(list(dist=distmat,opp=showmat))
}


makeLowResStruct <-
function(arfdata,experiment=.experiment)
#make low resolution structural image from high_res T1 image
{
	trials = list.files(.data.regDir(arfdata),full=F)
	
	for(trialdir in trials) {
		
		registration = loadRda(paste(.data.regDir(arfdata),.Platform$file.sep,trialdir,.Platform$file.sep,.data.regRda(arfdata),sep=''))
		
		examp = readData(.registration.linkedfile(registration))
		highres = readData(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.highres(registration),sep=''))
		
		ex2high = readFSLmat(paste(.registration.fullpath(registration),.Platform$file.sep,.registration.examp2high(registration),sep=''))
		
		dimx = .fmri.data.dims(examp)[2]
		dimy = .fmri.data.dims(examp)[3]
		dimz = .fmri.data.dims(examp)[4]
		
		highdat = .fmri.data.datavec(highres)
		dim(highdat) = c(.fmri.data.dims(highres)[2],.fmri.data.dims(highres)[3],.fmri.data.dims(highres)[4])
		
		newdat = examp
		.fmri.data.filename(newdat) = .experiment.lowresFile(experiment)
		.fmri.data.fullpath(newdat) = paste(.data.regDir(arfdata),.Platform$file.sep,trialdir,sep='')
		
		newdatavec = rep(0,dimx*dimy*dimz)
		dim(newdatavec) = c(dimx,dimy,dimz)		
		
		for(z in 1:dimz) {
			for(y in 1:dimy) {
				for(x in 1:dimx) {
							
					xyz = c(x,y,z,1)
					
					examp_mm = .registration.Dex(registration)%*%xyz
					high_mm = .registration.Aex2hi(registration)%*%examp_mm
					high_vox = solve(.registration.Dhi(registration))%*%high_mm
				
					newdatavec[x,y,z] = highdat[high_vox[1],high_vox[2],high_vox[3]] 

				}
			}
		}
		
		.fmri.data.cal_min(newdat) = min(newdatavec)
		.fmri.data.cal_max(newdat) = max(newdatavec)
		
		invisible(writeData(newdat,as.vector(newdatavec)))
		
	}
	
}

makeLowResStructAvg <-
function(arfmodel,experiment=.experiment)
#make average of low_resolution structural images
{
	
	sp = .Platform$file.sep
	trials = list.files(.model.regDir(arfmodel),full=F)
	
	avgdat = 0
	
	for(trialdir in trials) {
		
		registration = loadRda(paste(.model.regDir(arfmodel),sp,trialdir,sp,.model.regRda(arfmodel),sep=''))
		fn = list.files(path=.registration.fullpath(registration),pattern=.experiment.lowresFile(experiment),full=T)
		lrdat = readData(fn[1])
		avgdat = avgdat + .fmri.data.datavec(lrdat)
	}

	avgdat = avgdat / length(trials)
	
	avgstruct = lrdat
	.fmri.data.filename(avgstruct) = .experiment.lowresAvg(experiment)
	.fmri.data.fullpath(avgstruct) = .model.modeldatapath(arfmodel)
	.fmri.data.cal_min(avgstruct) = min(avgdat)
	.fmri.data.cal_max(avgstruct) = max(avgdat)

	invisible(writeData(avgstruct,avgdat))
}


getTal <-
function(atlas='talairach',FSLDIR='/usr/local/fsl',which='2mm')
#get talairach indices from FSL talairach file
{
	sp <- .Platform$file.sep
	
	atlas = match.arg(atlas)
	if(!atlas=='talairach') stop('only talairach atlas is supported')
	
	atlaspath = paste(FSLDIR,sp,'data/atlases',sep='')
	taldat = read.table(paste(atlaspath,sp,atlas,'.xml',sep=''),fill=T,skip=15,sep='\n',strings=F)
	
	talmat = matrix(NA,1106,6)
	
	for(i in 1:1106) {
	
		#split up data
		splits = strsplit(strsplit(taldat[i,1],'>')[[1]],'<')
		
		index = as.numeric(strsplit(strsplit(splits[[1]],' ')[[2]],'=')[[2]][2])
		namevec = strsplit(splits[[2]][1],"\\.")[[1]]
		
		talmat[i,1]=index
		talmat[i,2:6]=namevec
		
	}
	
	talname = paste(FSLDIR,sp,'data/atlases',sp,atlas,sp,atlas,'-labels-2mm.nii.gz',sep='')
	talfile = readData(talname)

	talmap = .fmri.data.datavec(talfile)
	dim(talmap) = c(.fmri.data.dims(talfile)[2],.fmri.data.dims(talfile)[3],.fmri.data.dims(talfile)[4])
	
	return(list(data=talmat,map=talmap))
		
}

getOxfordCort <-
function(atlas='HarvardOxford',FSLDIR='/usr/local/fsl',which='2mm')
#get talairach indices from FSL talairach file
{
	sp <- .Platform$file.sep
	
	atlas = match.arg(atlas)
	if(!atlas=='HarvardOxford') stop('only Oxford atlas is supported')
	
	atlaspath = paste(FSLDIR,sp,'data/atlases',sep='')
	taldat = read.table(paste(atlaspath,sp,atlas,'-Cortical.xml',sep=''),fill=T,skip=16,sep='\n',strings=F,quote="")
	taldat=apply(taldat,2,function(x) gsub("\"",'',x))
	
	talmat = matrix(NA,48,2)
	
	for(i in 1:48) {
		
		#split up data
		splits = strsplit(strsplit(taldat[i,1],'>')[[1]],'<')
		index = as.numeric(strsplit(strsplit(splits[[1]],' ')[[2]],'=')[[2]][2])
		namevec = strsplit(splits[[2]][1],"\\.")[[1]]

		talmat[i,1]=index
		talmat[i,2]=namevec
		
	}
	
	talname = paste(FSLDIR,sp,'data/atlases',sp,atlas,sp,atlas,'-cort-maxprob-thr0-2mm.nii.gz',sep='')
	talfile = readData(talname)

	talmap = .fmri.data.datavec(talfile)
	dim(talmap) = c(.fmri.data.dims(talfile)[2],.fmri.data.dims(talfile)[3],.fmri.data.dims(talfile)[4])

	return(list(data=talmat,map=talmap))
	
}

getOxfordSubCort <-
function(atlas='HarvardOxford',FSLDIR='/usr/local/fsl',which='2mm')
#get talairach indices from FSL talairach file
{
	sp <- .Platform$file.sep
	
	atlas = match.arg(atlas)
	if(!atlas=='HarvardOxford') stop('only Oxford atlas is supported')
	
	atlaspath = paste(FSLDIR,sp,'data/atlases',sep='')
	taldat = read.table(paste(atlaspath,sp,atlas,'-Subcortical.xml',sep=''),fill=T,skip=16,sep='\n',strings=F,quote="")
	taldat=apply(taldat,2,function(x) gsub("\"",'',x))
	
	talmat = matrix(NA,21,2)
	
	for(i in 1:21) {
		
		#split up data
		splits = strsplit(strsplit(taldat[i,1],'>')[[1]],'<')
		index = as.numeric(strsplit(strsplit(splits[[1]],' ')[[2]],'=')[[2]][2])
		namevec = strsplit(splits[[2]][1],"\\.")[[1]]
		
		talmat[i,1]=index
		talmat[i,2]=namevec
		
	}
	
	talname = paste(FSLDIR,sp,'data/atlases',sp,atlas,sp,atlas,'-sub-maxprob-thr0-2mm.nii.gz',sep='')
	talfile = readData(talname)
	
	talmap = .fmri.data.datavec(talfile)
	dim(talmap) = c(.fmri.data.dims(talfile)[2],.fmri.data.dims(talfile)[3],.fmri.data.dims(talfile)[4])
	
	return(list(data=talmat,map=talmap))
	
}



arfToAtlas <-
function(xyz,registration,atlasdata,which)
#gets atlas labels from ARF coordinates
{
	
	mni = arfToMNI(xyz,registration)
	atlas = round(MNI2atlas(mni,registration))

	if(any(atlas<1) | atlas[1]>91 | atlas[2]>109 | atlas[3]>91) index=0 else index = atlasdata$map[atlas[1],atlas[2],atlas[3]]
	
	if(which=='Talairach') return(atlasdata$data[index+1,])
	if(which=='HarvardOxford') 	if(index==0) return(c('-1','No label found!')) else return(try(atlasdata$data[index,]))
	
	
}


arfLocations <-
function(arfmodel,atlas=c('Talairach','HarvardOxford'))
#get and print talairach labels of region centers
{

	sp <- .Platform$file.sep

	atlas = match.arg(atlas)
	
	centers = round(matrix(.model.estimates(arfmodel),10)[c(1,2,3),])
	
	trial = list.files(.model.regDir(arfmodel),full=F)[1]
	registration = loadRda(paste(.model.regDir(arfmodel),sp,trial,sp,.model.regRda(arfmodel),sep=''))
	
	cat('Using',atlas,'atlas\n')
	
		if(atlas=='Talairach') {
		atlasdata = getTal()
		for(regs in 1:ncol(centers)) {
			
			regdat = arfToAtlas(as.vector(centers[,regs]),registration,atlasdata,atlas)
			cat('Region',regs,'[',centers[,regs],'] labels:\n')
			for(i in 1:(length(regdat)-1)) cat('  [',i,']  ',regdat[i+1],'\n',sep='')
			cat('\n')
			
		}
	}
	
	
	if(atlas=='HarvardOxford') {
		atlasdata_cort = getOxfordCort()
		atlasdata_sub = getOxfordSubCort()
	
		for(regs in 1:ncol(centers)) {
			
			regdatC = arfToAtlas(as.vector(centers[,regs]),registration,atlasdata_cort,atlas)
			regdatS = arfToAtlas(as.vector(centers[,regs]),registration,atlasdata_sub,atlas)
			
			cat('Region',regs,'[',centers[,regs],'] labels:\n')
			cat('  [cort]  ',regdatC[2],'\n',sep='')
			cat('  [sub ]  ',regdatS[2],'\n',sep='')
			cat('\n')
			
		}
	}
		
}



