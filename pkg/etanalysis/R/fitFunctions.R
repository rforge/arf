
ssq <- function(theta,datavec,weightvec,nreg,dimx,dimy) {
	## ssq is the objective function (sums-of-squares)
	## it calls the external C-funtion 'ssq'
	## input are theta (paramters), datavec, weightvec, number of regions, and dim x and dim y
	## output is a vector of parameter estimates (double)
	
	nlmdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(nreg),as.integer(dimx),as.integer(dimy),as.double(vector('numeric',1)))
	
	return(invisible(nlmdat[[7]]))	
	
}

modelest <- function(estimates,dimx,dimy)
{
	model <- .C('gauss',as.double(estimates),as.integer(length(estimates)),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]
	return(model)
}


fitModel <- function(datmap,reg,startval=NULL,iterlim=10000,trace=10)
{
	
	dimx = dim(datmap)[2]
	dimy = dim(datmap)[1]
	
	if(is.null(startval)) startval <- rep(c(20,20,2,3,.05,100),reg)
	
	optim.output <- try(suppressWarnings(optim(
							startval,
							ssq, 	#objective function
							#lower=lowbound, 
							#upper=upbound,
							datavec=as.vector(t(datmap)), #data
							weightvec=rep(1,dimx*dimy), #weight
							nreg=reg*6, #numparam
							dimx=dimx, #dimx
							dimy=dimy, #dimy
							method='BFGS',
							control=list(trace=trace,maxit=iterlim), #control
							hessian=T
					)),silent=F)
	
	return(list(model=optim.output,data=datmap))
	
}

calcBIC <- function(mod) 
{
	
	reg = length(mod$model$par)/6
	n = dim(mod$data)[1]*dim(mod$data)[2]
	minmod = mod$model$value
	weights = rep(1,n)
	
	#calculate the determinant of the weights
	dtm <- prod(weights)
	
	#check if determinant is valid
	if(!is.na(dtm) & !is.nan(dtm)) {
		if(is.numeric(try(log(n))) & is.numeric(try(log(dtm))) & is.numeric(log(minmod))) {
			if(log(dtm)==-Inf) {dtm=1e-323;warning('Determinant set to minimum value 1e-323.')}
			if(log(dtm)==Inf) {dtm=1e308;warning('Determinant set to maximum value 1e308.')}
			
			cons <- (2*(((n/2)*log(2*pi))+((1/2)*log(dtm))+((1/2)*(minmod)))) 
			
		} else {
			warning('Error calculating BIC. BIC not calculated\n')
		}
	} else {
		warning('Invalid determinant. BIC not calculated\n')
	} 
	
	#check if constant is a number and calculate BIC
	if(is.numeric(cons)) {
		bic <- cons + (log(minmod)) + (((reg*6))*log(n))
	} else {
		warning('Constant invalid. BIC not calculated\n')
	} 
	
	return(bic)
	
}