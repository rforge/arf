`BIC` <-
function(arfmodel) {
	## BIC calculates the Bayesian Information Fit criterion
	## input is an object of arfmodel
	## output is an object of arfmodel
		
	if(.model.valid(arfmodel)) {
		
		#read in weights, used in constant
		Wdata <- readData(.model.avgWfile(arfmodel))
		
		#warn if data is more than 2D
		if(.fmri.data.dims(Wdata)[1]>2) warning('BIC only works on 2D data, only first two dimensions are used.')
		
		n <- .fmri.data.dims(Wdata)[2]*.fmri.data.dims(Wdata)[3]
		data <- .fmri.data.datavec(Wdata)[1:n]
		
		#calculate the determinant of the weights
		dtm <- prod(data)
						
		#check if determinant is valid
		if(!is.na(dtm) & !is.nan(dtm)) {
			if(is.numeric(try(log(n))) & is.numeric(try(log(dtm))) & is.numeric(log(.model.minimum(arfmodel)))) {
				if(log(dtm)==-Inf) {dtm=1e-323;warning('Determinant set to minimum value 1e-323.')}
				if(log(dtm)==Inf) {dtm=1e308;warning('Determinant set to maximum value 1e308.')}
		
				cons <- (2*(((n/2)*log(2*pi))+((1/2)*log(dtm))+((1/2)*(.model.minimum(arfmodel))))) 
 
			} else {
				.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'Error calculating BIC. BIC not calculated\n',sep='')
				.model.valid(arfmodel) <- FALSE
			}
		} else {
			.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'Invalid determinant. BIC not calculated\n',sep='')
			.model.valid(arfmodel) <- FALSE
		} 
		
		#check if constant is a number and calculate BIC
		if(is.numeric(cons)) {
			.model.fit(arfmodel) <- cons + (log(.model.minimum(arfmodel))) + (((.model.regions(arfmodel)*6))*log(n))
		} else {
			.model.warnings(arfmodel) <- paste(.model.warnings(arfmodel),'Constant invalid. BIC not calculated\n',sep='')
			.model.valid(arfmodel) <- FALSE
		} 
 		
	} else	warning('No valid model. BIC not calculated.')
	
	return(invisible(arfmodel))
	
}

