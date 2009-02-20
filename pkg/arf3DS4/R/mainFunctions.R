#############################################
# arf3DS4 MAIN FUNCTIONS (initialize, arf)	#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


arf <- function(arfdata,regions=1,name='',exp=.experiment,options=new('options')) {
	
	cat('Activated Region Fitting\n')

	#check class of data and experiment
	if(class(arfdata)!='data') stop('input must be of class \'data\'.\n')
	if(class(exp)!='experiment') stop('input must be of class \'experiment\'.\n')
	
	#set the name of the model
	if(name=='') name <- paste('model_',regions,'_region',sep='')
		
	#create a new model
	arfmodel <- newModel(arfdata,experiment=exp,modelname=name,options=options)
	
	.model.regions(arfmodel) <- regions
	.model.sandwichmethod(arfmodel) <- .options.sw.type(options)
	
	cat('Creating averages...')
	arfdata <- createAverages(exp,arfdata)
	cat('OK\n')
	
	cat('Determining starting values...')
	arfmodel <- determineStartRect(arfmodel)
	cat('OK\n')
	
	cat('Fitting model...')
	arfmodel <- fitModel(arfmodel)
	cat('OK\n')
	
	cat('Calculating BIC...')
	arfmodel <- BIC(arfmodel)
	cat('OK\n')

	cat('Calculating derivatives...')
	makeDerivs(arfmodel)
	cat('OK\n')
	
	cat('Calculating residuals...')
	makeResiduals(arfmodel)
	cat('OK\n')
	
	cat('Calculating variance/covariance matrix...')
	arfmodel <- varcov(arfmodel)
	cat('OK\n')
	
	cat('Calculating Wald stats...')
	arfmodel <- wald(arfmodel)
	cat('OK\n')
	
	cat('Saving model...')
	saveModel(arfmodel)
	cat('OK\n')
	
	return(invisible(arfmodel))
}


