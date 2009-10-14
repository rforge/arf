#############################################
# arf3DS4 S4 FITMODEL FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#ssq.gauss
#model.gauss
#gradient.gauss
#ssq.simple
#model.simple
#gradient.simple
#createAverages
#createAllAverages
#determineStartRect
#determineStartRectSimple
#isEdge
#fallOff
#fwhm.filter
#setMask
#validStart
#checkBound


ssq.gauss <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
##ssq.gauss returns the ssq of the full gauss model with an anlytical gradient attached
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		ssqdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(brain),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[9]]
	} else ssqdat=ss_data
	
	if(analyticalgrad) {
		grad = gradient.gauss(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data)
		attr(ssqdat,'gradient') <- grad
	}
	
	if(is.nan(ssqdat) | ssqdat==Inf | is.na(ssqdat) | ssqdat==-Inf) ssqdat=ss_data
	#cat('ssqdat',ssqdat,'\n')
	return(invisible(ssqdat))	
	
}

model.gauss <-
function(theta,np,dimx,dimy,dimz)
#returns model estimate for full gaussmodel
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		model <- .C('gauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	} else model=NA
	
	return(model)
	
}

gradient.gauss <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
#gradient returns the analytical gradient of the ssq to the thetaparameters
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0) {
		model <- .C('gauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	} else model=NA
	
	if(length(model[is.na(model) | is.nan(model) | model==Inf | model==-Inf])==0) {
		grad <- .C('dfssq',as.integer(np),as.integer(brain),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[10]]
	} else grad=rep(1e+12,np) 
	
	#cat('gradient',grad,'\n')
	return(grad)

}

ssq.simple <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
##ssq.simple returns the ssq of the simple gauss model with an anlytical gradient attached
{
	ssqdat <- .C('simplessqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(brain),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[9]]
	
	if(analyticalgrad) {
		grad = gradient.simple(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data)
		attr(ssqdat,'gradient') <- grad
	}
	
	if(is.nan(ssqdat) | ssqdat==Inf | is.na(ssqdat) | ssqdat==-Inf) ssqdat=ss_data
	
	return(invisible(ssqdat))	
	
}

model.simple <-
function(theta,np,dimx,dimy,dimz)
#returns model estimate for simple gaussmodel
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		model <- .C('simplegauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	} else model=NA
	
	return(model)
	
}

gradient.simple <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
#gradient returns the analytical gradient of the ssq to the thetaparameters
{
	model <- .C('simplegauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	grad <- try(.C('dfsimplessq',as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[9]],silen=T)
	
	if(!is.null(attr(grad,'class'))) grad=rep(1e+12,np) 
	
	return(grad)
	
}

ssq.gauss.rpr <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
##ssq.gauss returns the ssq of the full gauss model with an anlytical gradient attached
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		eigen <- makeEigen(theta,np) 
		if(!any(is.na(eigen))) ssqdat <- .C('ssqgaussrpr',as.double(theta),as.double(eigen),as.double(datavec),as.double(weightvec),as.integer(brain),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[10]]
		else ssqdat=ss_data
	} else ssqdat=ss_data
	
	if(analyticalgrad) {
		grad = gradient.gauss(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data)
		attr(ssqdat,'gradient') <- grad
	}
	
	if(is.nan(ssqdat) | ssqdat==Inf | is.na(ssqdat) | ssqdat==-Inf) ssqdat=ss_data
	#cat('ssqdat',ssqdat,'\n')
	return(invisible(ssqdat))	
	
}

model.gauss.rpr <-
function(theta,np,dimx,dimy,dimz)
#returns model estimate for full gaussmodel
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		eigen <- makeEigen(theta,np) 
		if(!any(is.na(eigen))) model <- .C('gaussrpr',as.double(theta),as.double(eigen),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[7]]
			else model=NA
	} else model=NA
	
	return(model)
	
}

gradient.gauss.rpr <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
#gradient returns the analytical gradient of the ssq to the thetaparameters
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0) {
		eigen <- makeEigen(theta,np) 
		if(!any(is.na(eigen))) model <- .C('gaussrpr',as.double(theta),as.double(eigen),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[7]]
			else model=NA
	} else model=NA
	
	if(length(model[is.na(model) | is.nan(model) | model==Inf | model==-Inf])==0) {
		grad <- .C('dfssq',as.integer(np),as.integer(brain),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[10]]
	} else grad=rep(1e+12,np) 
	
	return(grad)
	
}

ssq.simple.rpr <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
##ssq.simple returns the ssq of the simple gauss model with an anlytical gradient attached
{
	ssqdat <- .C('simplessqgaussrpr',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(brain),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[9]]

	if(analyticalgrad) {
		grad = gradient.simple(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data)
		attr(ssqdat,'gradient') <- grad
	}
	
	if(is.nan(ssqdat) | ssqdat==Inf | is.na(ssqdat) | ssqdat==-Inf) ssqdat=ss_data

	return(invisible(ssqdat))	
	
}

model.simple.rpr <-
function(theta,np,dimx,dimy,dimz)
#returns model estimate for simple gaussmodel
{
	if(length(theta[is.na(theta) | is.nan(theta) | theta==Inf | theta==-Inf])==0)  {
		model <- .C('simplegaussrpr',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	} else model=NA
	
	return(model)
	
}

gradient.simple.rpr <- 
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
#gradient returns the analytical gradient of the ssq to the thetaparameters
{
	model <- .C('simplegaussrpr',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(rep(0,dimx*dimy*dimz)))[[6]]
	grad <- try(.C('dfsimplessq',as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[9]],silen=T)
	
	if(!is.null(attr(grad,'class'))) grad=rep(1e+12,np) 
	#cat('grad',grad,'\n')
	return(grad)
	
}


makeEigen <- 
function(theta,np)
#calculate maximal eigenvalues for the sigma matrix of the spatial Gaussian
{
	est = matrix(theta,10)
	eigen = numeric(np/10)
	for(i in 1:(np/10)) {
		sigma = diag(est[4:6,i])
		sigma[2,1]=sigma[1,2]=est[7,i]*sigma[1,1]*sigma[2,2]
		sigma[3,1]=sigma[1,3]=est[8,i]*sigma[1,1]*sigma[3,3]
		sigma[2,3]=sigma[3,2]=est[9,i]*sigma[3,3]*sigma[2,2]
		
		ev = try(eigen(sigma,T,T),silen=F)
		
		if(class(ev)=='list') {
			eigen[i] = round(max(abs(ev$values)))
		} else {
			eigen[i]=NA
		}
	}
	return(eigen)
}

createAverages <- 
function(arfdat,experiment=NULL) 
# createAverages averages of the data and weightfiles, set n mask and ss_data 
{
	
	#check experiment
	if(is.null(experiment)) if(exists('.experiment')) experiment = .experiment else stop('Experiment not loaded. Run loadExp first.')
	
	#set filesep
	sp=.Platform$file.sep
	
	# add trial data to avgdat and weightdat
	avgdat <- avgweight <- 0
	for(i in 1:.data.trials(arfdat)) {
		avgdat <- avgdat + .fmri.data.datavec(readData(.data.betafiles(arfdat)[i]))
		avgweight <- avgweight + .fmri.data.datavec(readData(.data.weightfiles(arfdat)[i]))
	}
	
	# divide avgdat by trialnumber and avgweight by trialnumber^2
	avgdat <- avgdat / .data.trials(arfdat)
	avgweight <- avgweight / .data.trials(arfdat)^2
	
	# get header info of first file (or reference file id supplied)
	headinf <- readHeader(getFileInfo(.data.betafiles(arfdat)[1])) 
	
	# write header and datainfo for datafiles
	filename <- paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sp,.experiment.avgdatFile(experiment),'.',.nifti.header.extension(headinf),sep='')
	
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'Average data image (ARF)'
	.data.avgdatfile(arfdat) <- filename
	writeData(headinf,avgdat)
	
	# get header info of first file (or reference file id supplied)
	headinf <- readHeader(getFileInfo(.data.weightfiles(arfdat)[1])) 
	
	# write header and datainfo for weightfiles
	filename <- paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sp,.experiment.avgWFile(experiment),'.',.nifti.header.extension(headinf),sep='')
	
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'Average weights image (ARF)'
	.data.avgWfile(arfdat) <- filename	
	writeData(headinf,avgweight)
	
	#save t-image
	filename <- paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sp,.experiment.avgtstatFile(experiment),'.',.nifti.header.extension(headinf),sep='')
	
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'Average t-statistics image (ARF)'
	.data.avgtstatFile(arfdat) <- filename
	
	avgtstat <- avgdat/sqrt(avgweight)
	avgtstat[is.nan(avgtstat)]=0
	
	writeData(headinf,avgtstat)
	
	
	#define mask
	brain <- rep(1,length(avgtstat))
	nb <- which(avgtstat==0)
	if(length(nb)>0) brain[nb]=0
	.data.mask(arfdat) <- brain
	
	#define n
	.data.n(arfdat) <- sum(brain) 
	
	#get ssq_data
	.data.ss(arfdat) <- .C('ssqdata',as.double(avgdat),as.double(avgweight),as.integer(brain),as.integer(length(avgtstat)),as.double(numeric(1)))[[5]]
	
	#save arfdatafile with updated weights
	save(arfdat,file=paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep=''))
	
	# return data class object	
	return(invisible(arfdat))
	
}

createAllAverages <- 
function(experiment=NULL) 
#create All averages is a wrapper to create all averages in an experiment
{
	
	#check experiment
	if(is.null(experiment)) if(exists('.experiment')) experiment = .experiment else stop('Experiment not loaded. Run loadExp first.')
	
	#set filesep
	sp <- .Platform$file.sep
		
	#set subjects and conditions
	subs <- .experiment.subject.names(experiment)
	conds <- .experiment.condition.names(experiment)
	
	#run through all subs and conds
	for(subject in subs) {
		for(condition in conds) {
			
			#make filename
			filename <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,subject,sp,.experiment.conditionDir(experiment),sp,condition,sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep='')
			
			#check and create, else warning
			if(file.exists(filename)) {
				createAverages(loadRda(filename))
			} else {
				warning(paste(filename,'does not exist. Avg not created'))
			}
		}
		
	}
	
	return(invisible(TRUE))
}



determineStartRect <- 
function(arfmodel,options=loadOptions(arfmodel)) 
# determineStartRect calculates starting values for regions (rectangular mode)
{
	
	.model.modeltype(arfmodel) <- 'gauss'
	.model.params(arfmodel) <- 10
	
	#load in fmriData
	fmridata <- readData(.model.avgtstatFile(arfmodel))
	
	#set theta to the default values (for all regions)
	theta <- rep(.options.start.vector(options),.model.regions(arfmodel))
	
	#set dimensions and read in data
	dimx <- .fmri.data.dims(fmridata)[2]
	dimy <- .fmri.data.dims(fmridata)[3]
	dimz <- .fmri.data.dims(fmridata)[4]
	data <- .fmri.data.datavec(fmridata)[1:(dimx*dimy*dimz)]
	
	mindim=c(1,1,1)
	maxdim=c(dimx,dimy,dimz)
	min_amp = .options.start.vector(options)[10]*-1
	max_amp = .options.start.vector(options)[10]
	
	#set dims of the data
	dim(data) <- c(dimx,dimy,dimz)
	
	#set location matrix
	location <- data.frame(x=rep(seq(1:dimx),times=dimz*dimy),y=rep(rep(seq(1:dimy),each=dimx),times=dimz),z=rep(seq(1:dimz),each=dimx*dimy))
	
	#set mask
	mask <- .model.mask(arfmodel)
	dim(mask) <-  c(dimx,dimy,dimz)
	
	for(reg in 1:.model.regions(arfmodel)) {
		
		#retrieve location in x,y,z
		m <- location[which.max(abs(data)),]
		if(data[m$x,m$y,m$z]<0) theta[10+(10*(reg-1))]=min_amp else theta[10+(10*(reg-1))]=max_amp
		
		#set maximum locations
		theta[1+(10*(reg-1))] <- m$x
		theta[2+(10*(reg-1))] <- m$y
		theta[3+(10*(reg-1))] <- m$z
				
		#caluclatefalloff
		xf <- fallOff(data[,m$y,m$z],.options.start.maxfac(options))
		yf <- fallOff(data[m$x,,m$z],.options.start.maxfac(options))
		zf <- fallOff(data[m$x,m$y,],.options.start.maxfac(options))
		
		#set width in x,y and z dirs
		theta[4+(10*(reg-1))] <- round(mean(xf))
		theta[5+(10*(reg-1))] <- round(mean(yf))
		theta[6+(10*(reg-1))] <- round(mean(zf))
		
		#check widths for zeroes
		theta[4+(10*(reg-1))][theta[4+(10*(reg-1))]<=0]=1 
		theta[5+(10*(reg-1))][theta[5+(10*(reg-1))]<=0]=1 
		theta[6+(10*(reg-1))][theta[6+(10*(reg-1))]<=0]=1 
		
		#zero tha data
		xvec <- (m$x-xf[1]):(m$x+xf[2])
		yvec <- (m$y-yf[1]):(m$y+yf[2])
		zvec <- (m$z-zf[1]):(m$z+zf[2])
		
		rmx <- which(xvec<mindim[1] | xvec>maxdim[1])
		rmy <- which(yvec<mindim[2] | yvec>maxdim[2])
		rmz <- which(zvec<mindim[3] | zvec>maxdim[3])
		
		if(length(rmx)>0 & length(rmx)<length(xvec)) xvec <- xvec[-rmx]
		if(length(rmy)>0 & length(rmy)<length(yvec)) yvec <- yvec[-rmy]
		if(length(rmz)>0 & length(rmz)<length(zvec)) zvec <- zvec[-rmz]	
		
		data[xvec,yvec,zvec]=0

	}
	
	
	#save startingvalues
	.model.startval(arfmodel) <- theta
	saveStart(.model.startval(arfmodel),arfmodel)
	
	#save startmap
	.fmri.data.fullpath(fmridata) <- .model.modeldatapath(arfmodel)
	.fmri.data.filename(fmridata) <- 'startmap'
	.fmri.data.intent_name(fmridata) <- 'start_search_map'
	writeData(fmridata,as.vector(data))
	
	#save model
	saveModel(arfmodel)
	
	return(invisible(arfmodel))
	
}

determineStartRectSimple <- 
function(arfmodel,options=loadOptions(arfmodel)) 
# determineStartRect calculates starting values for regions (rectangular mode)
{
	
	.model.modeltype(arfmodel) <- 'simple'
	.model.params(arfmodel) <- 5
	
	#load in fmriData
	fmridata <- readData(.model.avgtstatFile(arfmodel))
	
	#set theta to the default values (for all regions)
	theta <- rep(.options.start.vector(options),.model.regions(arfmodel))
	newstart <- .model.regions(arfmodel)*5
	
	#set dimensions and read in data
	dimx <- .fmri.data.dims(fmridata)[2]
	dimy <- .fmri.data.dims(fmridata)[3]
	dimz <- .fmri.data.dims(fmridata)[4]
	data <- .fmri.data.datavec(fmridata)[1:(dimx*dimy*dimz)]
	
	mindim=c(1,1,1)
	maxdim=c(dimx,dimy,dimz)
	min_amp = .options.start.vector(options)[10]*-1
	max_amp = .options.start.vector(options)[10]
	
	#set dims of the data
	dim(data) <- c(dimx,dimy,dimz)
	
	#set location matrix
	location <- data.frame(x=rep(seq(1:dimx),times=dimz*dimy),y=rep(rep(seq(1:dimy),each=dimx),times=dimz),z=rep(seq(1:dimz),each=dimx*dimy))
	
	mask = .model.mask(arfmodel)
	dim(mask)=c(dimx,dimy,dimz)
	
	for(reg in 1:.model.regions(arfmodel)) {
		
		#retrieve location in x,y,z
		m <- location[which.max(abs(data)),]
		if(data[m$x,m$y,m$z]<0) theta[10+(10*(reg-1))]=min_amp else theta[10+(10*(reg-1))]=max_amp
				
		#set maximum locations
		theta[1+(10*(reg-1))] <- m$x
		theta[2+(10*(reg-1))] <- m$y
		theta[3+(10*(reg-1))] <- m$z
		
		#caluclatefalloff
		xf <- fallOff(data[,m$y,m$z],.options.start.maxfac(options))
		yf <- fallOff(data[m$x,,m$z],.options.start.maxfac(options))
		zf <- fallOff(data[m$x,m$y,],.options.start.maxfac(options))
		
		#set width in x,y and z dirs
		theta[4+(10*(reg-1))] <- round(mean(xf))
		theta[5+(10*(reg-1))] <- round(mean(yf))
		theta[6+(10*(reg-1))] <- round(mean(zf))
		
		#check widths for zeroes
		theta[4+(10*(reg-1))][theta[4+(10*(reg-1))]<=0]=1 
		theta[5+(10*(reg-1))][theta[5+(10*(reg-1))]<=0]=1 
		theta[6+(10*(reg-1))][theta[6+(10*(reg-1))]<=0]=1 
				
		#zero tha data
		xvec <- (m$x-xf[1]):(m$x+xf[2])
		yvec <- (m$y-yf[1]):(m$y+yf[2])
		zvec <- (m$z-zf[1]):(m$z+zf[2])
		
		rmx <- which(xvec<mindim[1] | xvec>maxdim[1])
		rmy <- which(yvec<mindim[2] | yvec>maxdim[2])
		rmz <- which(zvec<mindim[3] | zvec>maxdim[3])
		
		if(length(rmx)>0 & length(rmx)<length(xvec)) xvec <- xvec[-rmx]
		if(length(rmy)>0 & length(rmy)<length(yvec)) yvec <- yvec[-rmy]
		if(length(rmz)>0 & length(rmz)<length(zvec)) zvec <- zvec[-rmz]	
				
		data[xvec,yvec,zvec]=0
				
		newstart[1+(5*(reg-1))]=theta[1+(10*(reg-1))]
		newstart[2+(5*(reg-1))]=theta[2+(10*(reg-1))]
		newstart[3+(5*(reg-1))]=theta[3+(10*(reg-1))]
		newstart[4+(5*(reg-1))]=mean(theta[4+(10*(reg-1))],theta[5+(10*(reg-1))],theta[6+(10*(reg-1))])
		newstart[5+(5*(reg-1))]=theta[10+(10*(reg-1))]
			
	}
	
	
	#save startingvalues
	.model.startval(arfmodel) <- newstart
	saveStart(.model.startval(arfmodel),arfmodel)
	
	#save startmap
	.fmri.data.fullpath(fmridata) <- .model.modeldatapath(arfmodel)
	.fmri.data.filename(fmridata) <- 'startmap'
	.fmri.data.intent_name(fmridata) <- 'start_search_map'
	writeData(fmridata,as.vector(data))
	
	#save model
	saveModel(arfmodel)
	
	return(invisible(arfmodel))
	
}

isEdge <-  
function(mask,xvec,yvec,zvec)
#check if a cube-region touches non-brain voxels
{
	if(sum(as.vector(mask[xvec,yvec,zvec]))==length(as.vector(mask[xvec,yvec,zvec]))) return(FALSE) else return(TRUE)
		
}

fallOff <- 
function(vec,fwhm=2)
#calculates mean falloff of a vector
{
	#smooth vec with fwhm filter
	vec <- fwhm.filter(vec,fwhm)
	
	#determine max of vector and falloff amount
	m <- which.max(vec)
	maxval <- max(vec)
	falloffval <- maxval/2
	
	#set min and max-dim elements
	maxdim <- length(vec)
	mindim <- 1
	
	#check falloff to the right
	i=0
	while(vec[m+i]>falloffval) {
		i=i+1;		
		if((m+i)>=maxdim) break()
	}
	if(i>1) i=i-1
	
	#check falloff to the left
	j=0
	while(vec[m-j]>falloffval) {
		j=j+1;		
		if((m-j)<=mindim) break()
	}
	if(j>1) j=j-1
	
	#return left and right values
	return(c(j,i))
}

fwhm.filter <- 
function(vec,fwhm) 
#smooth a datavector (1D) using FWHM filter (used in startval detect)
{
	
	
	fl=50
	filt=dnorm(1:100,mean=50,sd=fwhm)
	
	fdat <- convolve(vec,filt,type='open')
	vec <- fdat[-c(1:(fl-1),(length(fdat)-(fl-1)):length(fdat))]
	
	return(vec)
	
}

setMask <- 
function(arfmodel) 
#set mask attributes if necessary
{
	
	avgtstat <- .fmri.data.datavec(readData(.model.avgtstatFile(arfmodel)))
	
	#define mask
	brain <- rep(1,length(avgtstat))
	nb <- which(avgtstat==0)
	if(length(nb)>0) brain[nb]=0
	.model.mask(arfmodel) <- brain
	
	#define n
	.model.n(arfmodel) <- sum(brain)
	
	#get ssq_data
	.model.ss(arfmodel) <- .C('ssqdata',as.double(.fmri.data.datavec(readData(.model.avgdatfile(arfmodel)))),as.double(.fmri.data.datavec(readData(.model.avgWfile(arfmodel)))),as.integer(brain),as.integer(length(avgtstat)),as.double(numeric(1)))[[5]]
	
	return(invisible(arfmodel))
}

validStart <- 
function(arfmodel) 
#check if startvalues are valid
{
	
	theta <- .model.startval(arfmodel)
	mess = character(0)
	onlywarn=TRUE
		
	dat=readData(.model.avgtstatFile(arfmodel))
	mask = .model.mask(arfmodel)
	dim(mask)=c(.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4])
	
	if(length(.model.startval(arfmodel))!=(.model.regions(arfmodel)*.model.params(arfmodel))) {
		mess=c(mess,'[startval] Vector of startingvalues are not a multiple of regions*parameters')
		onlywarn=FALSE
	} else {
		for(reg in 1:.model.regions(arfmodel)) {
			st_loc = theta[(1:3)+(.model.params(arfmodel)*(reg-1))]
			if(.model.modeltype(arfmodel)=='gauss') st_sd = theta[(4:6)+(.model.params(arfmodel)*(reg-1))]  else st_sd=rep(theta[(4)+(.model.params(arfmodel)*(reg-1))],3)
			if(.model.modeltype(arfmodel)=='gauss') st_rho = theta[(7:9)+(.model.params(arfmodel)*(reg-1))] else st_rho=rep(0,3)
			if(.model.modeltype(arfmodel)=='gauss') st_amp = theta[(10)+(.model.params(arfmodel)*(reg-1))] else st_amp=theta[(5)+(.model.params(arfmodel)*(reg-1))]
			
			sigma = matrix(c(st_sd[1]^2,st_sd[1]*st_sd[2]*st_rho[1],st_sd[1]*st_sd[3]*st_rho[2],st_sd[1]*st_sd[2]*st_rho[1],st_sd[2]^2,st_sd[2]*st_sd[3]*st_rho[3],st_sd[1]*st_sd[3]*st_rho[2],st_sd[2]*st_sd[3]*st_rho[3],st_sd[3]^2),3,3)
			det_sig=det(sigma)
			
			if(!is.na(det_sig) & !is.nan(det_sig) & det_sig!=Inf & det_sig!=-Inf) {
				if(det_sig<1 & det_sig>0) mess = c(mess,paste('[startval] Region ',reg,' has a small volume (',round(det_sig,2),').',sep=''))
				if(det_sig<=0) mess = c(mess,paste('[startval] Region ',reg,' has a zero volume (',round(det_sig,2),').',sep=''))
				
			} else {
				mess = c(mess,paste('[startval] Region ',reg,' returns NA/NaN/Inf/-Inf as determinant.',sep=''))
				onlywarn=FALSE
			}
			
			if(st_loc[1]<1 | st_loc[2]<1 | st_loc[3]<1) {
				mess=c(mess,paste('[startval] Region ',reg,' has a location smaller than 1',sep=''))
			} 
			
			if(st_amp==0) {
				mess=c(mess,paste('[startval] Region ',reg,' has an amplitude of zero.'))
				onlywarn=FALSE
			}
		}
		
	}
	
	if(length(mess)>0) {
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),mess)
		if(!onlywarn) .model.valid(arfmodel) <- FALSE else .model.valid(arfmodel) <- TRUE
	} else {
		.model.valid(arfmodel) <- TRUE
	}
	
	if(!.model.valid(arfmodel)) .model.convergence(arfmodel) <- 'Starting values are not valid. Minimization routine not started.'
	
	return(invisible(arfmodel))
	
}


checkBound <- 
function(arfmodel,lowbound,upbound,thres=8) 
#check if parameters are on the bound
{
	
	estimates = matrix(.model.estimates(arfmodel),.model.params(arfmodel))

	for(i in 1:.model.params(arfmodel)) {
		regs = which(round(estimates[i,],thres)==round(lowbound[i],thres) | round(estimates[i,],thres)==round(upbound[i],thres))
		
		if(length(regs)>0) {
			mess = paste('[optim] Parameter',i,'is at boundary for regions ',regs,'\n')
			.model.warnings(arfmodel) = c(.model.warnings(arfmodel),mess)
		}
		
	}
	
	return(arfmodel)
	
}