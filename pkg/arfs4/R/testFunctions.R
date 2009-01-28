# Test Functions (Model fit, Wald tests) 
# Wouter D. Weeda, University of Amsterdam
###############################################################################


BIC <- function(arfmodel) {
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


wald <- function(arfmodel,waldobject) {
	## wald calculates Wald statistics for a fitted model
	## input is and model object and waldobject (may be empty)
	## output is an arfmodel object
	
	#define function to calculate Wald statistic 
	W <- function(a,A,C) t(a)%*%solve(A%*%C%*%t(A))%*%a
				
	if(.model.valid(arfmodel)) {
	
		#if no design matrix is specified in the waldobject, make the default matrix (zero-filled) 
		if(dim(.wald.design(waldobject))[1]==0) .wald.design(waldobject) <- matrix(0,.model.regions(arfmodel),4)
		
		# get dimensions
		dims <- .nifti.header.dims(readHeader(getFileInfo(.model.avgdatfile(arfmodel))))
		n <- dims[2]*dims[3]
		
		#set relevant matrix sizes and dfs
		.wald.stats(waldobject) <- matrix(0,.model.regions(arfmodel),4)
		.wald.pvalues(waldobject) <- matrix(0,.model.regions(arfmodel),4)
		.wald.df1(waldobject) <- rep(n,4)
		.wald.df2(waldobject) <- c(1,1,3,1)
		
		#perform hypothesis tests for each region and for locations, extent and amplitude
		for(region in 1:.model.regions(arfmodel)) {
			
			#select the 6*6 vcov matrix and estimates for each region
			theta <- .model.estimates(arfmodel)[((1+(region-1)*6):(region*6))]
			C <- .model.varcov(arfmodel)[((1+(region-1)*6):(region*6)),((1+(region-1)*6):(region*6))]
			
			#define the a matrix (containing hypotheses), uses info from the designmatrix
			a <- c(theta[1]-.wald.design(waldobject)[region,1],theta[2]-.wald.design(waldobject)[region,2],(((theta[3]^2)*(theta[4]^2))-((theta[5]*theta[4]*theta[3])^2))-.wald.design(waldobject)[region,3],theta[6]-.wald.design(waldobject)[region,4])
			
			#define the A matrix (containing the derivatives of a
			A <- matrix(0,4,6)
			A[1,1] <- 1
			A[2,2] <- 1
			A[3,3] <- (2*(theta[3]*(theta[4]^2)))*(1-theta[5]^2)
			A[3,4] <- (2*(theta[4]*(theta[3]^2)))*(1-theta[5]^2)
			A[3,5] <- -2*(theta[5]*(theta[3]^2)*(theta[4]^2))
			A[4,6] <- 1
			
			#perform tests for locations (calc stats and p-values)
			.wald.stats(waldobject)[region,1] <- W(a[1],A[1,1],C[1,1])
			.wald.pvalues(waldobject)[region,1] <- 1-pf(.wald.stats(waldobject)[region,1],.wald.df1(waldobject)[1],.wald.df2(waldobject)[1])
			.wald.stats(waldobject)[region,2] <- W(a[2],A[2,2],C[2,2])
			.wald.pvalues(waldobject)[region,2] <- 1-pf(.wald.stats(waldobject)[region,2],.wald.df1(waldobject)[2],.wald.df2(waldobject)[2])
			
			#perform tests for spatial extent (calc stats and p-values)
			.wald.stats(waldobject)[region,3] <- W(a[3],matrix(A[3,(3:5)],1,3),C[(3:5),(3:5)])
			.wald.pvalues(waldobject)[region,3] <- 1-pf(.wald.stats(waldobject)[region,3],.wald.df1(waldobject)[3],.wald.df2(waldobject)[3])
		
			#perform tests for amplitude (calc stats and p-values)
			.wald.stats(waldobject)[region,4] <- W(a[4],A[4,6],C[6,6])
			.wald.pvalues(waldobject)[region,4] <- 1-pf(.wald.stats(waldobject)[region,4],.wald.df1(waldobject)[4],.wald.df2(waldobject)[4])
		}	
		
		.model.wald(arfmodel) <- waldobject	
	
	} else	warning('No valid model. wald statistics not calculated.')
	
	return(invisible(arfmodel))
	
}


