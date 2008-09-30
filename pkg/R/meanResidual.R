`meanResidual` <-
function(arfmodel) {
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
		resids <- .C("outerprod",as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]
		
		#calculate resids for second and further trials and sum
		for(i in 2:.model.trials(arfmodel)) {
			
			data <- .fmri.data.datavec(readData(.model.datafiles(arfmodel)[i]))[1:(dimx*dimy)]
			resids <- resids + .C("outerprod",as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]	
				
		}
	
		#divide resids by number of trials squared
		resids <- resids/.model.trials(arfmodel)^2
		
		
	} else warning('No valid model. Residuals not calculated.')
		
	return(invisible(resids))
}

