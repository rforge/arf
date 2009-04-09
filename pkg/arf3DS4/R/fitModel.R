#############################################
# arf3DS4 S4 FITMODEL FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


ssq <- function(theta,datavec,weightvec,np,dimx,dimy,dimz) {
	## ssq is the objective function (sums-of-squares)
	## it calls the external C-funtion 'ssq'
	## input are theta (paramters), datavec, weightvec, number of regions, and dim x and dim y
	## output is a vector of parameter estimates (double)
	
	nlmdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(np),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(vector('numeric',1)))
	
	return(invisible(nlmdat[[8]]))	
	
}

## fitModel calls the minimization routine (NLM)
fitModel <- function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel))) {
		
	#start_time
	st_time <- Sys.time()
	
	# call NLM (within a try-loop)
	nlm.output <- try(suppressWarnings(nlm(
					ssq,
					.model.startval(arfmodel),
					datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
					weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
					np=.model.regions(arfmodel)*10,
					dimx=.fmri.data.dims(dat)[2],
					dimy=.fmri.data.dims(dat)[3],
					dimz=.fmri.data.dims(dat)[4],
					print.level=0,
					hessian=T,
					iterlim=.options.min.iterlim(options)
					)),silen=T)
	
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
		.model.valid(arfmodel) <- FALSE	
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}

# createAverages averages the data and weightfiles 
createAverages <- function(experiment,arfdat) {

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
	
	# return data class object	
	return(invisible(arfdat))
	
}

# determineStartRect calculates starting values for regions (rectangular mode)
determineStartRect <- function(arfmodel,startvec=loadStart(arfmodel),options=loadOptions(arfmodel)) {

	#load in fmriData
	fmridata <- readData(.model.avgdatfile(arfmodel))
	
	#set theta to the default values (for all regions)
	theta <- rep(startvec,.model.regions(arfmodel))
			
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
	#determine max of vector and falloff amount
	m <- which.max(vec)
	maxval <- max(vec)
	falloffval <- maxval/fwhm
	
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
