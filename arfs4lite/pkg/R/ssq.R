`ssq` <-
function(theta,datavec,weightvec,nreg,dimx,dimy) {
	## ssq is the objective function (sums-of-squares)
	## it calls the external C-funtion 'ssq'
	## input are theta (paramters), datavec, weightvec, number of regions, and dim x and dim y
	## output is a vector of parameter estimates (double)
	
	nlmdat <- .C("ssqgauss",as.double(theta),as.double(datavec),as.double(weightvec),as.integer(nreg),as.integer(dimx),as.integer(dimy),as.double(vector('numeric',1)))
	
	return(invisible(nlmdat[[7]]))	
	
}

