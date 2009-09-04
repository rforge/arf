#############################################
# arf3DS4 S4 FITMODEL FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

ssq.gauss <- 
##ssq.gauss returns the ssq of the full gauss model with an anlytical gradient attached
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
{
	ssqdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(brain),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[9]]
	
	if(analyticalgrad) {
		grad = gradient.gauss(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data)
		attr(ssqdat,'gradient') <- grad
	}

	if(is.nan(ssqdat) | ssqdat==Inf | is.na(ssqdat) | ssqdat==-Inf) ssqdat=ss_data
	
	return(invisible(ssqdat))	
	
}

model.gauss <-
#returns model estimate for full gaussmodel
function(theta,np,dimx,dimy,dimz)
{
	return(.C('gauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',dimx*dimy*dimz)))[[6]])
	
}

gradient.gauss <- 
#gradient returns the analytical gradient of the ssq to the thetaparameters
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
{
	model <- .C('gauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',dimx*dimy*dimz)))[[6]]
	grad <- try(.C('dfssq',as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[9]],silen=T)
	
	if(!is.null(attr(grad,'class'))) grad=rep(1e+12,np) 
	
	return(grad)
	
}


ssq.simple <- 
##ssq.simple returns the ssq of the simple gauss model with an anlytical gradient attached
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
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
#returns model estimate for simple gaussmodel
function(theta,np,dimx,dimy,dimz)
{
	return(.C('simplegauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',dimx*dimy*dimz)))[[6]])
	
}

gradient.simple <- 
#gradient returns the analytical gradient of the ssq to the thetaparameters
function(theta,datavec,weightvec,brain,np,dimx,dimy,dimz,ss_data,analyticalgrad) 
{
	model <- .C('simplegauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',dimx*dimy*dimz)))[[6]]
	grad <- try(.C('dfsimplessq',as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(theta),as.double(datavec),as.double(model),as.double(weightvec),as.double(vector('numeric',np)))[[9]],silen=T)
	
	if(!is.null(attr(grad,'class'))) grad=rep(1e+12,np) 
	
	return(grad)
	
}

# createAverages averages of the data and weightfiles, set n mask and ss_data 
createAverages <- function(arfdat,experiment=.experiment) {
	
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
	.nifti.header.descrip(headinf) <- 'ARF average data'
	.data.avgdatfile(arfdat) <- filename
	writeData(headinf,avgdat)
	
	# get header info of first file (or reference file id supplied)
	headinf <- readHeader(getFileInfo(.data.weightfiles(arfdat)[1])) 
	
	# write header and datainfo for weightfiles
	filename <- paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sp,.experiment.avgWFile(experiment),'.',.nifti.header.extension(headinf),sep='')
	
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'ARF average weights'
	.data.avgWfile(arfdat) <- filename	
	writeData(headinf,avgweight)
	
	#save t-image
	filename <- paste(.data.fullpath(arfdat),sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sp,.experiment.avgtstatFile(experiment),'.',.nifti.header.extension(headinf),sep='')
	
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'ARF average t-stat'
	.data.avgtstatFile(arfdat) <- filename
	avgtstat <- avgdat/sqrt(avgweight)
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

createAllAverages <- function(experiment=.experiment) {
	
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


# determineStartRect calculates starting values for regions (rectangular mode)
determineStartRect <- function(arfmodel,startvec=loadStart(arfmodel),options=loadOptions(arfmodel)) {
	
	#load in fmriData
	fmridata <- readData(.model.avgdatfile(arfmodel))
	
	#set theta to the default values (for all regions)
	theta <- startvec
	
	#set dimensions and read in data
	dimx <- .fmri.data.dims(fmridata)[2]
	dimy <- .fmri.data.dims(fmridata)[3]
	dimz <- .fmri.data.dims(fmridata)[4]
	data <- .fmri.data.datavec(fmridata)[1:(dimx*dimy*dimz)]
	
	mindim=c(1,1,1)
	maxdim=c(dimx,dimy,dimz)
	
	#set dims of the data
	dim(data) <- c(dimx,dimy,dimz)
	
	#set location matrix
	location <- data.frame(x=rep(seq(1:dimx),times=dimz*dimy),y=rep(rep(seq(1:dimy),each=dimx),times=dimz),z=rep(seq(1:dimz),each=dimx*dimy))
	
	for(reg in 1:.model.regions(arfmodel)) {
		
		#retriev location in x,y,z
		m <- location[which.max(data),]
		
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
		
		#check determinant of sigma
		detsig = try(detSigmaDeriv(theta),silen=T)
		if(is.null(attr(detsig,'class'))) {
			if(detsig$value<((dimx*dimy*dimz)/1e+4)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('starting values of region',reg,'seem very small (determinant of sigma is small)'))
		} else {
			.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('starting values of region',reg,'return invalid deterimant of sigma'))
		}
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

# determineStartRect calculates starting values for regions (rectangular mode)
determineStartRectSimple <- function(arfmodel,options=loadOptions(arfmodel)) {
	
	#load in fmriData
	fmridata <- readData(.model.avgdatfile(arfmodel))
	
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
	
	#set dims of the data
	dim(data) <- c(dimx,dimy,dimz)
	
	#set location matrix
	location <- data.frame(x=rep(seq(1:dimx),times=dimz*dimy),y=rep(rep(seq(1:dimy),each=dimx),times=dimz),z=rep(seq(1:dimz),each=dimx*dimy))
	
	for(reg in 1:.model.regions(arfmodel)) {
		
		#retriev location in x,y,z
		m <- location[which.max(data),]
		
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
		
		#check determinant of sigma
		detsig = try(detSigmaDeriv(theta),silen=T)
		if(is.null(attr(detsig,'class'))) {
			if(detsig$value<((dimx*dimy*dimz)/1e+4)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('starting values of region',reg,'seem very small (determinant of sigma is small)'))
		} else {
			.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('starting values of region',reg,'return invalid deterimant of sigma'))
		}
		
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



#calculates mean falloff of a vector
fallOff <- function(vec,fwhm=2) 
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

#smooth a datavector using FWHM filter (used in startval detect)
fwhm.filter <- function(vec,fwhm) {
	
	len=length(vec)
	fl=50
	filt=dnorm(1:100,mean=50,sd=fwhm)
	
	fdat <- convolve(vec,filt,type='open')
	vec <- fdat[-c(1:(fl-1),(length(fdat)-(fl-1)):length(fdat))]
	
	return(vec)
	
}

#set mask attributes if necessary
setMask <- function(arfmodel) {
	
	avgtstat <- .fmri.data.datavec(readData(.model.avgtstatFile(arfmodel)))
	
	#define mask
	brain <- rep(1,length(avgtstat))
	nb <- which(avgtstat==0)
	if(length(nb)>0) brain[nb]=0
	.model.mask(arfmodel) <- brain
	
	#define n
	.model.n(arfmodel) <- sum(brain)
	
	#get ssq_data
	.model.ss(arfmodel) <- .C('ssqdata',as.double(.fmri.data.datavec(readData(.model.avgdatFile(arfmodel)))),as.double(.fmri.data.datavec(readData(.model.avgWFile(arfmodel)))),as.integer(brain),as.integer(length(avgstat)),as.double(numeric(1)))[[5]]
	
	return(invisible(arfmodel))
}


