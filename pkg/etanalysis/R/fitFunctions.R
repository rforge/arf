###############################################################################
# ET analysis Functions (Derivatives, Residuals, SandwichCalc) 
# Wouter D. Weeda, University of Amsterdam
###############################################################################

ssq <- function(theta,datavec,weightvec,nreg,dimx,dimy) {
	## ssq is the objective function (sums-of-squares)
	## it calls the external C-funtion 'ssq'
	## input are theta (paramters), datavec, weightvec, number of regions, and dim x and dim y
	## output is a vector of parameter estimates (double)
	
	nlmdat <- .C('ssqgauss',as.double(theta),as.double(datavec),as.double(weightvec),as.integer(nreg),as.integer(dimx),as.integer(dimy),as.double(vector('numeric',1)))
	
	return(invisible(nlmdat[[7]]))	
	
}

modelest <- function(mod)
{
	estimates <- mod$model$par
	dimx <- dim(mod$data)[2]
	dimy <- dim(mod$data)[1]
		
	model <- .C('gauss',as.double(estimates),as.integer(length(estimates)),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]
	model <- matrix(model,dimy,dimx,byrow=T)
	
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

calcFit <- function(mod) 
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
		aic <- cons + (log(minmod)) + 2*(reg*6)
		
	} else {
		warning('Constant invalid. BIC not calculated\n')
	} 
	
	return(list(aic=aic,bic=bic))
	
}


residDiag <- function(mod,datarr) {
	
			
	#open data for first trial to get dimensions and trial data
	data <- as.vector(datarr[,,1])
	tn <- dim(datarr)[3]
	
	#set dimensions and data
	dimx <- dim(datarr)[2]
	dimy <- dim(datarr)[1]
	n <- dimx*dimy
	
	#calculate model based on parameter estimates
	model <- .C('gauss',as.double(mod$model$par),as.integer(length(mod$model$par)),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]
	
	#caluclate outerproduct of residuals
	resids <- .C('outerproddiagonal',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]
	
	#calculate resids for second and further trials and sum
	for(i in 2:tn) {
		
		data <- datarr[,,i]
		resids <- resids + .C('outerproddiagonal',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]
		
	}
	
	#divide resids by number of trials squared
	resids <- resids/tn^2
	#resids <- resids/tn
	
	
	return(invisible(resids))	
	
}

residFull<- function(mod,datarr) {
	
	
	#open data for first trial to get dimensions and trial data
	data <- as.vector(datarr[,,1])
	tn <- dim(datarr)[3]
	
	#set dimensions and data
	dimx <- dim(datarr)[2]
	dimy <- dim(datarr)[1]
	n <- dimx^2*dimy^2
	
	#calculate model based on parameter estimates
	model <- .C('gauss',as.double(mod$model$par),as.integer(length(mod$model$par)),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]

	#caluclate outerproduct of residuals
	resids <- .C('outerprod',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]
	
	#calculate resids for second and further trials and sum
	for(i in 2:tn) {
		data <- datarr[,,i]
		resids <- resids + .C('outerprod',as.integer(dimx*dimy),as.double(data-model),as.double(vector('numeric',n)))[[3]]	
	}
	
	#divide resids by number of trials squared
	resids <- resids/tn^2
	
	
	return(invisible(resids))	
	
}

swDiag <- function(mod,datarr) {
	
	R <- residDiag(mod,datarr)
	p <- length(mod$model$par)
	F <- matrix(derivatives(mod),,p) 
		
	#read in average weights (with dims)
	weights <- rep(1,dim(datarr)[2]*dim(datarr)[1])
		
	n <- dim(datarr)[2]*dim(datarr)[1]
	W <- weights
	rm(weights)
		
	#calculate the inner sandwich part B (in A-1BA-1)
	B <- ((t(F)*(1/W))*R)%*%(F*(1/W))
		
	#calculate the sandwich estimator (using the Hessian returned by nlm)
	SW <- try(solve(.5*mod$model$hessian)%*%B%*%solve(.5*mod$model$hessian),silen=T)
		
	#check if alll went well and add to the arfmodel object
	if(is.null(attr(SW,'class'))) {
		varcov <- SW
	} else {
		warning('Failed to calculate Sandwich estimate.')
	}
	
	return(invisible(varcov))
	
}

swFull <- function(mod,datarr) {
	
	R <- residFull(mod,datarr)
	p <- length(mod$model$par)
	F <- matrix(derivatives(mod),,p) 
	
	#read in average weights (with dims)
	weights <- rep(1,dim(datarr)[2]*dim(datarr)[1])
	n <- dim(datarr)[2]*dim(datarr)[1]
	W <- weights
	rm(weights)
	
	#calculate the inner sandwich part B (in A-1BA-1)
	B <- matrix(.C('inner_sandwich',as.integer(n),as.integer(p),as.double(F),as.double(W),as.double(R),as.double(rep(0,p*p)))[[6]],p,p)
	
	#calculate the sandwich estimator (using the Hessian returned by nlm)
	SW <- try(solve(.5*mod$model$hessian)%*%B%*%solve(.5*mod$model$hessian),silen=T)
	
	#check if alll went well and add to the arfmodel object
	if(is.null(attr(SW,'class'))) {
		varcov <- SW
	} else {
		warning('Failed to calculate Sandwich estimate.')
	}
	
	return(invisible(varcov))
	
}


derivatives <- function(mod) {
			
	#read in average data
	data <- mod$data
	
	dimx <- dim(data)[2]
	dimy <- dim(data)[1]
	
	#calculate the derivatives
	deriv <- .C('fderiv2',as.integer(dimx*dimy),as.integer(length(mod$model$par)),as.integer(dimx),as.integer(dimy),as.double(mod$model$par),as.double(numeric(length(mod$model$par)*dimx*dimy)))[[6]]
			
	return(invisible(deriv))
}


wald <- function(mod,varcov) {
	
	#define function to calculate Wald statistic 
	W <- function(a,A,C) t(a)%*%solve(A%*%C%*%t(A))%*%a
	
	reg = length(mod$model$par)/6
	
	#if no design matrix is specified in the waldobject, make the default matrix (zero-filled) 
	desmat <- matrix(0,reg,4)
		
	# get dimensions
	n <- dim(mod$data)[2]*dim(mod$data)[1]
		
	#set relevant matrix sizes and dfs
	stats <- matrix(0,reg,4)
	pvals <- matrix(0,reg,4)
	df1 <- rep(n,4)
	df2 <- df1-rep(reg,4)
		
	#perform hypothesis tests for each region and for locations, extent and amplitude
	for(region in 1:reg) {
			
		#select the 6*6 vcov matrix and estimates for each region
		theta <- mod$model$par[((1+(region-1)*6):(region*6))]
		C <- varcov[((1+(region-1)*6):(region*6)),((1+(region-1)*6):(region*6))]
			
		#define the a matrix (containing hypotheses), uses info from the designmatrix
		a <- c(theta[1]-desmat[region,1],theta[2]-desmat[region,2],(((theta[3]^2)*(theta[4]^2))-((theta[5]*theta[4]*theta[3])^2))-desmat[region,3],theta[6]-desmat[region,4])
			
		#define the A matrix (containing the derivatives of a
		A <- matrix(0,4,6)
		A[1,1] <- 1
		A[2,2] <- 1
		A[3,3] <- (2*(theta[3]*(theta[4]^2)))*(1-theta[5]^2)
		A[3,4] <- (2*(theta[4]*(theta[3]^2)))*(1-theta[5]^2)
		A[3,5] <- -2*(theta[5]*(theta[3]^2)*(theta[4]^2))
		A[4,6] <- 1
			
		#perform tests for locations (calc stats and p-values)
		stats[region,1] <- W(a[1],A[1,1],C[1,1])
		pvals[region,1] <- 1-pf(stats[region,1],df1[1],df2[1])
		stats[region,2] <- W(a[2],A[2,2],C[2,2])
		pvals[region,2] <- 1-pf(stats[region,2],df1[2],df2[2])
		
		#perform tests for spatial extent (calc stats and p-values)
		stats[region,3] <- W(a[3],matrix(A[3,(3:5)],1,3),C[(3:5),(3:5)])
		pvals[region,3] <- 1-pf(stats[region,3],df1[3],df2[3])
			
		#perform tests for amplitude (calc stats and p-values)
		stats[region,4] <- W(a[4],A[4,6],C[6,6])
		pvals[region,4] <- 1-pf(stats[region,4],df1[4],df2[4])
	}	
		
	return(invisible(list(stats=stats,pvals=pvals,df1=df1,df2=df2)))
	
}

dStart <- function(data,regions,maxfac=2,svtemp=c(50,50,2,2,.01,100)) {
	
	#set dimensions and read in data
	dimx <- dim(data)[2]
	dimy <- dim(data)[2]
	data <- as.vector(data)
		
	#set theta to the default values (for all regions)
	theta <- rep(svtemp,regions)
	
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
		data = matrix(data,dimy,dimx,byrow=T)
		
		#find width in the x direction
		xhalfmax <- maxval/maxfac
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
		yhalfmax <- maxval/maxfac
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
