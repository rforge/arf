	#############################################
# arf3DS4 S4 FITMODEL FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#fitModel
#fitModelNlm
#fitModelOptim
#fitSimpleModelNlm
#fitSimpleModelOptim

fitModel <- 
function(arfmodel,type=c('gauss','simple'),options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
# fitModel is a wrapper for NLM and optim based on the options
{
	type = match.arg(type)
	
	if(.options.start.method(options)=='rect') {
		if(type=='gauss') arfmodel <- determineStartRect(arfmodel)
		if(type=='simple') arfmodel <- determineStartRectSimple(arfmodel) 	
	}
	
	if(.options.start.method(options)=='load') .model.startval(arfmodel) <- loadStart(arfmodel)
		
	if(.options.min.routine(options)=='nlm') {
		if(type=='simple')	arfmodel = fitSimpleModelNlm(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen) 
		if(type=='gauss')	arfmodel = fitModelNlm(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen) 
	}
	
	if(.options.min.routine(options)=='optim') {
		if(type=='simple')	arfmodel = fitSimpleModelOptim(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen) 
		if(type=='gauss')	arfmodel = fitModelOptim(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen) 
	}

	return(arfmodel)	
}


fitModelNlm <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
# fitModelNlm calls the minimization routine (NLM)
{
	
	#set filesep
	sp <- .Platform$file.sep
	
	#set modelobjects
	.options.min.routine(options) <- 'nlm'
	.model.modeltype(arfmodel) <- 'gauss'
	.model.params(arfmodel) <- 10
	
	#start_time
	st_time <- Sys.time()

	#check if averages exist
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- character(0)
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	
	#check startingvalues
	arfmodel <- validStart(arfmodel)
	
	#final check before fit
	if(!.model.valid(arfmodel)) {
		saveModel(arfmodel)	
		return(arfmodel)
	}
		
	#call NLM (within a try-loop)
	nlm.output <- try(suppressWarnings(nlm(
					ssq.gauss,
					.model.startval(arfmodel),
					datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
					weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
					np=.model.regions(arfmodel)*.model.params(arfmodel),
					brain=.model.mask(arfmodel),
					dimx=.fmri.data.dims(dat)[2],
					dimy=.fmri.data.dims(dat)[3],
					dimz=.fmri.data.dims(dat)[4],
					ss_data=.model.ss(arfmodel),
					analyticalgrad=.options.min.analyticalgrad(options),
					print.level=printlevel,
					hessian=T,
					check.analyticals=F,
					iterlim=.options.min.iterlim(options),
					gradtol=.options.nlm.gradtol(options),
					steptol=.options.nlm.steptol(options)
					)),silen=try.silen)
	
	#end_time
	en_time <- Sys.time()
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(nlm.output,'class'))) {
		if(nlm.output$code==1) .model.convergence(arfmodel) <- paste('Gradient close to zero. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==2) .model.convergence(arfmodel) <- paste('Iterates within tolerance. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==3) .model.convergence(arfmodel) <- 'No lower point found.' 
		if(nlm.output$code==4) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(nlm.output$code==5) .model.convergence(arfmodel) <- 'Stepmax exceeded five times. No convergence.'
		if(nlm.output$code <= 2) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
		
		#set model objects
		.model.minimum(arfmodel) <- nlm.output$minimum
		.model.hessian(arfmodel) <- nlm.output$hessian
		.model.estimates(arfmodel) <- nlm.output$estimate
		.model.iterates(arfmodel) <- nlm.output$iterations
		.model.sandwichmethod(arfmodel) <- .options.sw.type(options)
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		if(.options.min.analyticalgrad(options)) .model.gradient(arfmodel) <- gradient.gauss(.model.estimates(arfmodel),.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],.model.mask(arfmodel),.model.regions(arfmodel)*.model.params(arfmodel),.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4],.model.ss(arfmodel),analyticalgrad=T)
		
		if(.model.valid(arfmodel)) {
			#save the ModelBinary
			arfmodel <- saveModelBin(arfmodel)
						
			#save the weights in a binary file
			weights <- readData(.model.avgWfile(arfmodel))
			con <- file(paste(.model.modeldatapath(arfmodel),sp,.model.weightFile(arfmodel),sep=''),'wb')
			writeBin(.fmri.data.datavec(weights),con,double())
			close(con)
			
			#make Derivatives 
			makeDerivs(arfmodel)
			
			#create residuals
			makeResiduals(arfmodel)
			
			if(.options.min.analyticalgrad(options)) {
				df_fn <- paste(.model.modeldatapath(arfmodel),.Platform$file.sep,.model.derivativeFile(arfmodel),sep='')
				w_fn <- paste(.model.modeldatapath(arfmodel),.Platform$file.sep,.model.weightFile(arfmodel),sep='')
				n = .fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(weights)[4]
				p = .model.regions(arfmodel)*.model.params(arfmodel)
				hessian <- try(.C('approxHessian',as.integer(p),as.integer(n),as.character(df_fn),as.character(w_fn),as.double(numeric(p*p))),silen=try.silen)
				
				if(is.null(attr(hessian,'class'))) {
					hessian <- hessian[[5]]
					dim(hessian) = c(p,p)
					.model.hessian(arfmodel) <- hessian 	
				} else {
					.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),'[min] Could not approximate Hessian with analytical gradient.')
					.model.valid(arfmodel) <- FALSE
					
				}
			}
			
			#caluclate fits
			arfmodel = BIC(arfmodel,options=options)
			arfmodel = RMSEA(arfmodel,options=options)
			
		} else .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] nlm did not converge.',sep=''))
		
		
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] nlm internal error: ',nlm.output,sep=''))
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)

	#return arf model object	
	return(invisible(arfmodel))
}


fitModelOptim <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
# fitModelOptim calls the minimization routine (OPTIM)
{

	#set separator
	sp <- .Platform$file.sep
	
	#set modelobjects
	.options.min.routine(options) <- 'optim'
	.model.modeltype(arfmodel) <- 'gauss'
	.model.params(arfmodel) <- 10
	
	#start_time
	st_time <- Sys.time()
		
	#check if averages exist
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- character(0)
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	
	#set analyticalgrad options
	if(.options.min.analyticalgrad(options)) {
		gradfunc=gradient.gauss
		angrad=FALSE
	} else {
		gradfunc=NULL
		angrad=FALSE
	}
	
	#set boundaries in L-BFGS-B mode
	if(length(.options.opt.lower(options))==1 | length(.options.opt.upper(options))==1) {
		lowbound=-Inf
		upbound=Inf
	} else {
		#set location to maximal dim
		.options.opt.upper(options)[1:3] = c(.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4])
		
		#set width parameters to maxdim divided by tphe value given in the options
		.options.opt.upper(options)[4] = .fmri.data.dims(dat)[2]/.options.opt.upper(options)[4]
		.options.opt.upper(options)[5] = .fmri.data.dims(dat)[3]/.options.opt.upper(options)[5]
		.options.opt.upper(options)[6] = .fmri.data.dims(dat)[4]/.options.opt.upper(options)[6]
	
		upbound = rep(.options.opt.upper(options),.model.regions(arfmodel))
		lowbound = rep(.options.opt.lower(options),.model.regions(arfmodel))
	}

	#check startingvalues
	arfmodel <- validStart(arfmodel)
		
	#final check before fit
	if(!.model.valid(arfmodel)) {
		saveModel(arfmodel)	
		return(arfmodel)
	}
	
	#runoptim	
	optim.output <- try(suppressWarnings(optim(
						.model.startval(arfmodel),
						ssq.gauss,
						gradfunc,
						lower=lowbound,
						upper=upbound,
						datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
						weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
						brain=.model.mask(arfmodel),
						np=.model.regions(arfmodel)*.model.params(arfmodel),
						dimx=.fmri.data.dims(dat)[2],
						dimy=.fmri.data.dims(dat)[3],
						dimz=.fmri.data.dims(dat)[4],
						ss_data=.model.ss(arfmodel),
						analyticalgrad=angrad,
						method=.options.opt.method(options),
						control=list(trace=printlevel,maxit=.options.min.iterlim(options)),
						hessian=T
					)),silen=try.silen)

	#end_time
	en_time <- Sys.time()

	# check for internal errors and set relevant arf model values
	if(is.null(attr(optim.output,'class'))) {
		if(optim.output$convergence==0) .model.convergence(arfmodel) <- paste('Optim converged in ',optim.output$counts[1],' iterations.',sep='')
		if(optim.output$convergence==1) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(optim.output$convergence==10) .model.convergence(arfmodel) <- 'Degeneracy of the Nelder-Mead Simplex'
		if(optim.output$convergence==51) .model.convergence(arfmodel) <- paste('BFGS raises warning:',optim.output$message,sep='')
		if(optim.output$convergence==52) .model.convergence(arfmodel) <-  paste('BFGS raises error:',optim.output$message,sep='')
		
		if(optim.output$convergence <= 0) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
		
		#set model objects
		.model.minimum(arfmodel) <- optim.output$value
		.model.estimates(arfmodel) <- optim.output$par
		.model.hessian(arfmodel) <- optim.output$hessian
		.model.iterates(arfmodel) <- optim.output$counts[1]
		.model.sandwichmethod(arfmodel) <- .options.sw.type(options)
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		if(.options.min.analyticalgrad(options)) .model.gradient(arfmodel) <- gradient.gauss(.model.estimates(arfmodel),.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],.model.mask(arfmodel),.model.regions(arfmodel)*.model.params(arfmodel),.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4],.model.ss(arfmodel),analyticalgrad=T)
		
		
	
		if(.model.valid(arfmodel)) {
			#save the ModelBinary
			arfmodel <- saveModelBin(arfmodel)
			
			#save the weights in a binary file
			con <- file(paste(.model.modeldatapath(arfmodel),sp,.model.weightFile(arfmodel),sep=''),'wb')
			writeBin(.fmri.data.datavec(weights),con,double())
			close(con)
			
			#make Derivatives 
			makeDerivs(arfmodel)
			
			#create residuals
			makeResiduals(arfmodel)
			
			if(.options.min.analyticalgrad(options)) {
				df_fn <- paste(.model.modeldatapath(arfmodel),.Platform$file.sep,.model.derivativeFile(arfmodel),sep='')
				w_fn <- paste(.model.modeldatapath(arfmodel),.Platform$file.sep,.model.weightFile(arfmodel),sep='')
				n = .fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(weights)[4]
				p = .model.regions(arfmodel)*.model.params(arfmodel)
				hessian <- try(.C('approxHessian',as.integer(p),as.integer(n),as.character(df_fn),as.character(w_fn),as.double(numeric(p*p))),silen=try.silen)
				
				if(is.null(attr(hessian,'class'))) {
					hessian <- hessian[[5]]
					dim(hessian) = c(p,p)
					.model.hessian(arfmodel) <- hessian 	
				} else {
					.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),' [min] could not approximate Hessian with analytical gradient.')
					.model.valid(arfmodel) <- FALSE
					
				}
			}
			
			#caluclate fits
			arfmodel = BIC(arfmodel,options=options)
			arfmodel = RMSEA(arfmodel,options=options)
			
		} else .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] optim did not converge.',sep=''))
			
		
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] optim internal error: ',optim.output,sep=''))
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}

fitSimpleModelOptim <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
# fitModelOptim calls the minimization routine (OPTIM)
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#set routine to optim
	.options.min.routine(options) <- 'optim'
	.model.modeltype(arfmodel) <- 'simple'
	.model.params(arfmodel) <- 5
	
	#start_time
	st_time <- Sys.time()
	
	#check if averages exist
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- character(0)
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	
	.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),'Simple Gaussmodel was fitted.')
	
	#set analyticalgrad options
	if(.options.min.analyticalgrad(options)) {
		gradfunc=gradient.simple
		angrad=FALSE
	} else {
		gradfunc=NULL
		angrad=FALSE
	}
	
	#set boundaries in L-BFGS-B mode
	#set boundaries in L-BFGS-B mode
	if(length(.options.opt.lower(options))==1 | length(.options.opt.upper(options))==1) {
		lowbound=-Inf
		upbound=Inf
	} else {
		#set location to maximal dim
		.options.opt.upper(options)[1:3] = c(.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4])
		
		#set width parameters to maxdim divided by tphe value given in the options
		.options.opt.upper(options)[4] = .fmri.data.dims(dat)[2]/.options.opt.upper(options)[4]
		.options.opt.upper(options)[5] = .fmri.data.dims(dat)[3]/.options.opt.upper(options)[5]
		.options.opt.upper(options)[6] = .fmri.data.dims(dat)[4]/.options.opt.upper(options)[6]
		
		upbound = rep(.options.opt.upper(options)[-c(5:9)],.model.regions(arfmodel))
		lowbound = rep(.options.opt.lower(options)[-c(5:9)],.model.regions(arfmodel))
		
	}	
	
	#check startingvalues
	arfmodel <- validStart(arfmodel)
	
	#final check before fit
	if(!.model.valid(arfmodel)) {
		saveModel(arfmodel)	
		return(arfmodel)
	}
	
	#runoptim	
	optim.output <- try(suppressWarnings(optim(
							.model.startval(arfmodel),
							ssq.simple,
							gradfunc,
							lower=lowbound,
							upper=upbound,
							datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
							weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
							brain=.model.mask(arfmodel),
							np=.model.regions(arfmodel)*.model.params(arfmodel),
							dimx=.fmri.data.dims(dat)[2],
							dimy=.fmri.data.dims(dat)[3],
							dimz=.fmri.data.dims(dat)[4],
							ss_data=.model.ss(arfmodel),
							analyticalgrad=angrad,
							method=.options.opt.method(options),
							control=list(trace=printlevel,maxit=.options.min.iterlim(options)),
							hessian=F
					)),silen=try.silen)
	
	#end_time
	en_time <- Sys.time()
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(optim.output,'class'))) {
		if(optim.output$convergence==0) .model.convergence(arfmodel) <- paste('Optim converged in ',optim.output$counts[1],' iterations.',sep='')
		if(optim.output$convergence==1) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(optim.output$convergence==10) .model.convergence(arfmodel) <- 'Degeneracy of the Nelder-Mead Simplex'
		if(optim.output$convergence==51) .model.convergence(arfmodel) <- paste('BFGS raises warning:',optim.output$message,sep='')
		if(optim.output$convergence==52) .model.convergence(arfmodel) <-  paste('BFGS raises error:',optim.output$message,sep='')
		
		if(optim.output$convergence <= 0) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
		
		#set model essentials
		.model.estimates(arfmodel) <- optim.output$par
		.model.iterates(arfmodel) <- optim.output$counts[1]
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		
		if(.model.valid(arfmodel)) {
		
			
			#save the ModelBinary
			arfmodel <- saveModelBinSimple(arfmodel)
			
			#set model objects
			.model.minimum(arfmodel) <- optim.output$value
			.model.estimates(arfmodel) <- rep(0,.model.regions(arfmodel)*10)
			
			for(i in 1:.model.regions(arfmodel)) {
				.model.estimates(arfmodel)[1+(10*(i-1))] <- optim.output$par[1+(5*(i-1))]
				.model.estimates(arfmodel)[2+(10*(i-1))] <- optim.output$par[2+(5*(i-1))]
				.model.estimates(arfmodel)[3+(10*(i-1))] <- optim.output$par[3+(5*(i-1))]
				.model.estimates(arfmodel)[4+(10*(i-1))] <- optim.output$par[4+(5*(i-1))]
				.model.estimates(arfmodel)[5+(10*(i-1))] <- optim.output$par[4+(5*(i-1))]
				.model.estimates(arfmodel)[6+(10*(i-1))] <- optim.output$par[4+(5*(i-1))]
				.model.estimates(arfmodel)[7+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[8+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[9+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[10+(10*(i-1))] <- optim.output$par[5+(5*(i-1))]
			}
					
			if(.options.min.analyticalgrad(options)) .model.gradient(arfmodel) <- gradient.simple(.model.estimates(arfmodel),.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],.model.mask(arfmodel),.model.regions(arfmodel)*.model.params(arfmodel),.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4],.model.ss(arfmodel),analyticalgrad=T)
		
			if(.model.valid(arfmodel)) {
				#caluclate fits
				arfmodel = BIC(arfmodel,options=options)
				arfmodel = RMSEA(arfmodel,options=options)
			}
		} else .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] optim did not converge.',sep=''))
		
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] optim internal error: ',optim.output,sep=''))
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}


fitSimpleModelNlm <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
# fitModelOptim calls the minimization routine (OPTIM)
{

	#set separator
	sp <- .Platform$file.sep
	
	#set routine to nlm
	.options.min.routine(options) <- 'nlm'
	.model.modeltype(arfmodel) <- 'simple'
	.model.params(arfmodel) <- 5
		
	#start_time
	st_time <- Sys.time()

	#check if averages exist
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- character(0)
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),'Simple Gaussmodel was fitted.')
	
	#check startingvalues
	arfmodel <- validStart(arfmodel)
	
	#final check before fit
	if(!.model.valid(arfmodel)) {
		saveModel(arfmodel)	
		return(arfmodel)
	}
	
	#call NLM (within a try-loop)
	nlm.output <- try(suppressWarnings(nlm(
							ssq.simple,
							.model.startval(arfmodel),
							datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
							weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
							np=.model.regions(arfmodel)*.model.params(arfmodel),
							brain=.model.mask(arfmodel),
							dimx=.fmri.data.dims(dat)[2],
							dimy=.fmri.data.dims(dat)[3],
							dimz=.fmri.data.dims(dat)[4],
							ss_data=.model.ss(arfmodel),
							analyticalgrad=.options.min.analyticalgrad(options),
							print.level=printlevel,
							hessian=F,
							check.analyticals=F,
							iterlim=.options.min.iterlim(options),
							gradtol=.options.nlm.gradtol(options),
							steptol=.options.nlm.steptol(options)
					)),silen=try.silen)
	
	#end_time
	en_time <- Sys.time()
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(nlm.output,'class'))) {
		if(nlm.output$code==1) .model.convergence(arfmodel) <- paste('Gradient close to zero. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==2) .model.convergence(arfmodel) <- paste('Iterates within tolerance. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==3) .model.convergence(arfmodel) <- 'No lower point found.' 
		if(nlm.output$code==4) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(nlm.output$code==5) .model.convergence(arfmodel) <- 'Stepmax exceeded five times. No convergence.'
		if(nlm.output$code <= 2) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
	
		#set model essentials
		.model.estimates(arfmodel) <- nlm.output$estimates
		.model.iterates(arfmodel) <- nlm.output$iterations
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		
		if(.model.valid(arfmodel)) {
			#save the ModelBinary
			arfmodel <- saveModelBinSimple(arfmodel)
			
			#set model objects
			.model.minimum(arfmodel) <- nlm.output$minimum
			.model.estimates(arfmodel) <- rep(0,.model.regions(arfmodel)*10)
			for(i in 1:.model.regions(arfmodel)) {
				.model.estimates(arfmodel)[1+(10*(i-1))] <- nlm.output$estimate[1+(5*(i-1))]
				.model.estimates(arfmodel)[2+(10*(i-1))] <- nlm.output$estimate[2+(5*(i-1))]
				.model.estimates(arfmodel)[3+(10*(i-1))] <- nlm.output$estimate[3+(5*(i-1))]
				.model.estimates(arfmodel)[4+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[5+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[6+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[7+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[8+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[9+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[10+(10*(i-1))] <- nlm.output$estimate[5+(5*(i-1))]
			}
			
			if(.options.min.analyticalgrad(options)) .model.gradient(arfmodel) <- gradient.simple(.model.estimates(arfmodel),.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],.model.mask(arfmodel),.model.regions(arfmodel)*.model.params(arfmodel),.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4],.model.ss(arfmodel),analyticalgrad=T)
			
					
			if(.model.valid(arfmodel)) {
				arfmodel = BIC(arfmodel,options=options)
				arfmodel = RMSEA(arfmodel,options=options)
			}
		}  else .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] nlm did not converge.',sep=''))
		
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),paste('[min] nlm internal error: ',nlm.output,sep=''))
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),optim.output)
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
}
