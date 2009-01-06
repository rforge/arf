`sandwich` <-
function(arfmodel) {
	## sandwich calculates the sandwich variance/covariance estimator
	## input is an object of class arfmodel
	## output is an object of class arfmodel
		
	if(.model.valid(arfmodel)) {
		
		cat(' calculating sandwich estimates...')
		
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
		
		cat('done\n')
	
	} else {
		warning('No valid model. sandwich not calculated')
		.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'no var/covar matrix calculated.\n')
	}
	
	return(invisible(arfmodel))
}

