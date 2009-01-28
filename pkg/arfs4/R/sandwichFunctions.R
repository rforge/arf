# Sandwich Functions (Derivatives, Residuals, SandwichCalc) 
# Wouter D. Weeda, University of Amsterdam
###############################################################################


meanResidual <- function(arfmodel) {
	## meanResidual calculates the mean residual matrix used in the sandwich estimation
	## input is an object of class model
	## output is a vector of mean residuals
		
	if(.model.valid(arfmodel)) {
						
		#open data for first trial to get dimensions and trial data
		trialdata <- readData(.model.datafiles(arfmodel)[1])
		
		#warn if data is more than 2D
		if(.fmri.data.dims(trialdata)[1]>2) warning('residualCalc only works on 2D data, only first two dimensions are used.')
		
		#set dimensions and data
		dimx <- .fmri.data.dims(trialdata)[2]
		dimy <- .fmri.data.dims(trialdata)[3]
		data <- .fmri.data.datavec(trialdata)[1:(dimx*dimy)]
		n <- dimx^2*dimy^2
		rm(trialdata)
		
		#calculate model based on parameter estimates
		model <- .C('gauss',as.double(.model.estimates(arfmodel)),as.integer(.model.regions(arfmodel)*6),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]
				
		#caluclate outerproduct of residuals
		resids <- .C('outerprod',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]
				
		#calculate resids for second and further trials and sum
		for(i in 2:.model.trials(arfmodel)) {
			
			data <- .fmri.data.datavec(readData(.model.datafiles(arfmodel)[i]))[1:(dimx*dimy)]
			resids <- resids + .C('outerprod',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]	
				
		}
	
		#divide resids by number of trials squared
		resids <- resids/.model.trials(arfmodel)^2
		
		
	} else warning('No valid model. Residuals not calculated.')
		
	return(invisible(resids))
}


derivatives <- function(arfmodel) {
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

sandwich <- function(arfmodel) {
	## sandwich calculates the sandwich variance/covariance estimator
	## input is an object of class arfmodel
	## output is an object of class arfmodel
		
	if(.model.valid(arfmodel)) {
		
		#calculate residuals, derivatives and the number of parameters	
		R <- meanResidual(arfmodel)
		F <- derivatives(arfmodel) 
		p <- .model.regions(arfmodel)*6
		
		#read in average weights (with dims)
		weights <- readData(.model.avgWfile(arfmodel))
		if(.fmri.data.dims(weights)[1]>2) warning('sandwich only works on 2D data, only first two dimensions are used.')
		n <- (.fmri.data.dims(weights)[2])*(.fmri.data.dims(weights)[3])
		W <- .fmri.data.datavec(weights)[1:n]
		rm(weights)
					
		#calculate the inner sandwich part B (in A-1BA-1)
		B <- matrix(.C('inner_sandwich',as.integer(n),as.integer(p),as.double(F),as.double(W),as.double(R),as.double(rep(0,p*p)))[[6]],p,p)
		
		#calculate the sandwich estimator (using the Hessian returned by nlm)
		SW <- try(solve(.5*.model.hessian(arfmodel))%*%B%*%solve(.5*.model.hessian(arfmodel)),silen=T)
		
		#check if alll went well and add to the arfmodel object
		if(is.null(attr(SW,'class'))) {
			.model.varcov(arfmodel) <- SW
		} else {
			warning('Failed to calculate Sandwich estimate.')
			.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'no var/covar matrix calculated.\n')
			.model.valid(arfmodel) <- FALSE
		}
		
	} else {
		warning('No valid model. sandwich not calculated')
		.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'no var/covar matrix calculated.\n')
	}
	
	return(invisible(arfmodel))
}
