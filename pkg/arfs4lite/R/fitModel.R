`fitModel` <-
function(arfdata,startval,nreg,settings,dat=readData(.data.avgdatfile(arfdata)),weights=readData(.data.avgWfile(arfdata))) {
	## fitModel calls the minimization routine (NLM)
	## input is an arf data object, starting values, and number of regions to fit
	## as a default it reads in the averaged fmri data, but this can be overridden	
	## output is an arf model object (with convergence info, hessian and parameter estimates)
	
	# create new instance of arfmodel
	arfmodel=new('model',arfdata)
	
	# warn if dimensions of fmri data are > 2
	if(.fmri.data.dims(dat)[1]>2) warning('fitModel works only on 2D data, only first two dimensions will be used.')
	
	#set startingvalues and number of regions of arf model object
	.model.startval(arfmodel) <- startval
	.model.regions(arfmodel) <- nreg
	.model.modelname(arfmodel)=paste('region_',nreg,sep='')
	
	#display initial values
	time1 <- displayFitStart(arfmodel)
		
	# call NLM (within a try-loop)
	nlm.output <- try(nlm(
					ssq,
					.model.startval(arfmodel),
					datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3])],
					weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3])],
					nreg=.model.regions(arfmodel)*6,
					dimx=.fmri.data.dims(dat)[2],
					dimy=.fmri.data.dims(dat)[3],
					print.level=0,
					hessian=T,
					iterlim=.settings.min.iterlim(settings)
					),silen=T)
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(nlm.output,'class'))) {
		if(nlm.output$code==1) .model.convergence(arfmodel) <- 'Gradient close to zero, converged.'
		if(nlm.output$code==2) .model.convergence(arfmodel) <- 'Iterates within tolerance, converged.'
		if(nlm.output$code==3) .model.convergence(arfmodel) <- 'No lower point found. No convergence.'
		if(nlm.output$code==4) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(nlm.output$code==5) .model.convergence(arfmodel) <- 'Stepmax exceeded five times. No convergence.'
		if(nlm.output$code <= 2) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
		.model.minimum(arfmodel) <- nlm.output$minimum
		.model.hessian(arfmodel) <- nlm.output$hessian
		.model.estimates(arfmodel) <- nlm.output$estimate
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.valid(arfmodel) <- FALSE	
	}
	
	displayFitEnd(arfmodel,time1)
	displayEstimates(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}

