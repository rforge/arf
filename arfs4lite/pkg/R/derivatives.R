`derivatives` <-
function(arfmodel) {
	## derivatives calculates the first order derivatives of the parameters to the objective function
	## input is an object of class model
	## output is a nxp matrix of derivatives
	
	if(.model.valid(arfmodel)) {
		
		#read in average data
		fmridata <- readData(.model.avgdatfile(arfmodel))
	
		#warn if data has more than 2 dimensions
		if(.fmri.data.dims(fmridata)[1]>2) warning('derivatives only works on 2D data, only first two dimensions are used.')
	
		dimx <- .fmri.data.dims(fmridata)[2]
		dimy <- .fmri.data.dims(fmridata)[3]
		
		#calculate the derivatives
		deriv <- .C('fderiv',as.integer(dimx*dimy),as.integer(.model.regions(arfmodel)*6),as.integer(dimx),as.integer(dimy),as.double(.model.estimates(arfmodel)),as.double(numeric(.model.regions(arfmodel)*6*dimx*dimy)))[[6]]
				
	} else	warning('No valid model. Derivatives not calculated.')

	return(invisible(deriv))
	
}

