# ARF Data Functions (Create Averages, Determine starts, Calculate Residuals)
# Wouter D. Weeda, University of Amsterdam
###############################################################################


createAverages <- function(arfdat,refheaddat=NULL,refheadW=NULL) {
	## createAverages averages the data and weightfiles 
	## input is an object of Data class
	## output is logical (datafiles are saved in avg directory)
	
	# check if avg directory exists
	if(!file.exists(paste(.data.fullpath(arfdat),'avg/',sep=''))) dir.create(paste(.data.fullpath(arfdat),'avg/',sep=''))
	
	# add trial data to avgdat and weightdat
	avgdat <- avgweight <- 0
	for(i in 1:.data.trials(arfdat)) {
		avgdat <- avgdat + .fmri.data.datavec(readData(.data.datafiles(arfdat)[i]))
		avgweight <- avgweight + .fmri.data.datavec(readData(.data.weightfiles(arfdat)[i]))
	}
	
	# divide avgdat by trialnumber and avgweight by trialnumber^2
	avgdat <- avgdat / .data.trials(arfdat)
	avgweight <- avgweight / .data.trials(arfdat)^2

	# get header info of first file (or reference file id supplied)
	if(is.null(refheaddat)) headinf <- readHeader(getFileInfo(.data.datafiles(arfdat)[1])) else headinf <- readHeader(getFileInfo(refheaddat))
	
	# write header and datainfo for datafiles
	filename <- paste(.data.fullpath(arfdat),'avg/avgdata.',.nifti.header.extension(headinf),sep='')
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'ARF average data'
	.data.avgdatfile(arfdat) <- filename
	writeData(headinf,avgdat)
	
	# get header info of first file (or reference file id supplied)
	if(is.null(refheadW)) headinf <- readHeader(getFileInfo(.data.weightfiles(arfdat)[1])) else headinf <- readHeader(getFileInfo(refheadW))
	
	# write header and datainfo for weightfiles
	filename <- paste(.data.fullpath(arfdat),'avg/avgweight.',.nifti.header.extension(headinf),sep='')
	if(.nifti.header.gzipped(headinf)==T) filename <- paste(filename,'.gz',sep='') 
	headinf <- newFile(filename,headinf)
	.nifti.header.descrip(headinf) <- 'ARF average weights'
	.data.avgWfile(arfdat) <- filename	
	writeData(headinf,avgweight)
	
	# return data class object	
	return(invisible(arfdat))
	
}


determineStart <- function(fmridata,settings,regions) {
	## determineStart calculates starting values for regions
	## values are determined for center locations (theta1 and theta2)
	## and widths (theta3 and theta4) other values are from settings
	## input is fmridata (2D), settings object and the number of regions
	## output is a vector of startingvalues
	
	#warn if more than 2D data
	if(.fmri.data.dims(fmridata)[1]>2) warning('determineStart only works on 2D data, only first two dimensions are used.')
	
	#set dimensions and read in data
	dimx <- .fmri.data.dims(fmridata)[2]
	dimy <- .fmri.data.dims(fmridata)[3]
	data <- .fmri.data.datavec(fmridata)[1:(dimx*dimy)]
	rm(fmridata)
	
	#set theta to the default values (for all regions)
	theta <- rep(.settings.start.vector(settings),regions)
	
	#create vectors for locations (x increases fastest)
	x <- rep(1:dimx,times=dimy)
	y <- rep(1:dimy,each=dimx)
	
	#set minimum value
	minval=min(data)
	
	for(reg in 1:regions) {
		
		#find maximum and set maxval
		maxwhich <- which.max(data)
		maxval <- max(data)
		
		#set theta 1 and 2 to locations of max
		theta[1+6*(reg-1)] <- x[maxwhich]
		theta[2+6*(reg-1)] <- y[maxwhich]
		
		#set dimensions of vector to dimx and dimy 
		dim(data) <- c(dimx,dimy)
		
		#find width in the x direction
		xhalfmax <- maxval/.settings.start.maxfac(settings)
		xcurrval <- maxval
		xcurrpos <- theta[1+6*(reg-1)]

		if(!(xcurrpos==dimx)) {
			while(xcurrval>xhalfmax) {
				xcurrpos <- xcurrpos+1
				xcurrval <- data[xcurrpos,theta[2+6*(reg-1)]]
				if(xcurrpos>=dimx) break
			}
		} 
				
		theta[3+6*(reg-1)] <- xcurrpos-theta[1+6*(reg-1)]
		if(theta[3+6*(reg-1)]<1) theta[3+6*(reg-1)]=1
		
		#find width in the y direction
		yhalfmax <- maxval/.settings.start.maxfac(settings)
		ycurrval <- maxval
		ycurrpos <- theta[2+6*(reg-1)]
		
		if(!(ycurrpos==dimy)) {
			while(ycurrval>yhalfmax) {
				ycurrpos <- ycurrpos+1
				ycurrval <- data[theta[1+6*(reg-1)],ycurrpos]
				if(ycurrpos>=dimy) break
			}
		} 
		
		theta[4+6*(reg-1)] <- ycurrpos-theta[2+6*(reg-1)]
		if(theta[4+6*(reg-1)]<1) theta[4+6*(reg-1)]=1
		
		#void field creation (empty already searched locations)
		xmin <- x[maxwhich]-theta[3+6*(reg-1)]
		xmax <- x[maxwhich]+theta[3+6*(reg-1)]
		ymin <- y[maxwhich]-theta[4+6*(reg-1)]
		ymax <- y[maxwhich]+theta[4+6*(reg-1)]
		
		#check if width locations don't fall of boundary
		if(xmin<1) xmin <- 1
		if(xmax>dimx) xmax <- dimx
		if(ymin<1) ymin <- 1
		if(ymax>dimy) ymax <- dimy
		
		#apply void field to data
		data[xmin:xmax,ymin:ymax]=(2*minval)
		
		#vectorize data
		data <- as.vector(data)
	}
	
	#return vector of starting values
	return(invisible(theta))
	
}




