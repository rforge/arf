#############################################
# arf3DS4 S4 FITMODEL FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


ssq <- function(theta,datavec,weightvec,np,dimx,dimy,dimz) {
	## ssq is the objective function (sums-of-squares)
	## it calls the external C-funtion 'ssq'
	## input are theta (parameters), datavec, weightvec, number of regions, and dim x and dim y
	## output is a vector of parameter estimates (double)
	
	nlmdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))[[8]]
	
	#cat(nlmdat)
	#if(!is.nan(nlmdat) & !is.na(nlmdat)) if(nlmdat==-1) nlmdat=NaN
	#cat(nlmdat,'\n')		
	
	#x=seq(0,np,10)[-length(seq(0,np,10))]
	
	#for(i in x) {
	#	sig_x=theta[4+i]^2
	#	sig_xy=theta[7+i]*theta[5+i]*theta[4+i];
	#	sig_xz=theta[8+i]*theta[6+i]*theta[4+i];
	#	sig_y=theta[5+i]^2
	#	sig_yz=theta[9+i]*theta[5+i]*theta[6+i];
	#	sig_z=theta[6+i]^2
	
	#	det_sig=sig_x*sig_y*sig_z-sig_x*sig_yz*sig_yz-sig_xy*sig_xy*sig_z+sig_xy*sig_xz*sig_yz+sig_xz*sig_xy*sig_yz-sig_xz*sig_y*sig_xz;
	
	#	if(theta[4+i] < 0 | theta[5+i] < 0  | theta[6+i] < 0) nlmdat=NaN
	#	if(theta[7+i] < (-.99) | theta[8+i] < (-.99)  | theta[9+i] < (-.99)) nlmdat=NaN
	#	if(theta[7+i] > (.99) | theta[8+i]  > (.99)  | theta[9+i] > (.99)) nlmdat=NaN
		
		
	#	cat(det_sig,' ')
	#}

	#cat(nlmdat,'\n')
	
	#browser()
	return(invisible(nlmdat))	
	
}

#modePred returns an array with modelpredictions for the model or the startingvalues
modelPred <- function(arfmodel,which=c('model','start')) {
	
	which <- match.arg(which[1],c('model','start'))
	
	if(which=='model') theta <- .model.estimates(arfmodel) else theta <- .model.startval(arfmodel)
	
	dat <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	dimx <- .nifti.header.dims(dat)[2]
	dimy <- .nifti.header.dims(dat)[3]
	dimz <- .nifti.header.dims(dat)[4]
	
	np <- .model.regions(arfmodel)*10
		
	model <- .C('gauss',as.double(theta),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',dimx*dimy*dimz)))[[6]]
	
	dim(model) <- c(dimx,dimy,dimz)
	
	return(model)
	
}

## fitModel calls the minimization routine (NLM)
fitModel <- function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel))) {
	
	sp <- .Platform$file.sep
	
	#start_time
	st_time <- Sys.time()
	
	#check if averages exist else stop
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- ''
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	
		
	#call NLM (within a try-loop)
	nlm.output <- try(suppressWarnings(nlm(
					ssq,
					.model.startval(arfmodel),
					datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
					weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
					np=.model.regions(arfmodel)*10,
					dimx=.fmri.data.dims(dat)[2],
					dimy=.fmri.data.dims(dat)[3],
					dimz=.fmri.data.dims(dat)[4],
					print.level=2,
					hessian=T,
					iterlim=.options.min.iterlim(options),
					gradtol=.options.min.gradtol(options),
					steptol=.options.min.steptol(options)
					)),silen=F)
	
	#end_time
	en_time <- Sys.time()
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(nlm.output,'class'))) {
		if(nlm.output$code==1) .model.convergence(arfmodel) <- paste('Gradient close to zero. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==2) .model.convergence(arfmodel) <- paste('Iterates within tolerance. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==3) .model.convergence(arfmodel) <- 'No lower point found. No convergence.'
		if(nlm.output$code==4) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(nlm.output$code==5) .model.convergence(arfmodel) <- 'Stepmax exceeded five times. No convergence.'
		if(nlm.output$code <= 2) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
		
		#set model objects
		.model.minimum(arfmodel) <- nlm.output$minimum
		.model.hessian(arfmodel) <- nlm.output$hessian
		.model.estimates(arfmodel) <- nlm.output$estimate
		.model.iterates(arfmodel) <- nlm.output$iterations
		.model.sandwichmethod(arfmodel) <- .options.sw.type(options)
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		
		#save the ModelBinary
		arfmodel <- saveModelBin(arfmodel)
				
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}

# createAverages averages the data and weightfiles 
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
	writeData(headinf,avgdat/sqrt(avgweight))
		
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
	rm(fmridata)

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
		
		data[xvec,yvec,zvec]=(min(data)/2)
	
	}
	
	.model.startval(arfmodel) <- theta
	
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


fwhm.filter <- function(vec,fwhm) {
	
	len=length(vec)
	fl=50
	filt=dnorm(1:100,mean=50,sd=fwhm)
	
	fdat <- convolve(vec,filt,type='open')
	vec <- fdat[-c(1:(fl-1),(length(fdat)-(fl-1)):length(fdat))]
		
	return(vec)
	
}