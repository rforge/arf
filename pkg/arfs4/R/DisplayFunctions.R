# Display Functions
# Wouter D. Weeda, University of Amsterdam
###############################################################################



displayFitStart <- function(arfmodel) {
	
	cat(' [ ',toupper(.model.modelname(arfmodel)),' ]','\n\n',sep='')
	cat('  [procedure]\n')
	cat('   fitting ',.model.regions(arfmodel),' region(s)\n',sep='')
	cat('   starting values:\n')
	for(i in 1:.model.regions(arfmodel)) cat(paste('    [',i,']',sep=''),.model.startval(arfmodel)[((1+(i-1)*6):(i*6))],'\n')
	
	time1=as.POSIXlt(Sys.time())
	
	if(round(time1$sec)<10) secs <- paste('0',round(time1$sec),sep='') else secs <- round(time1$sec)
	if(time1$min<10) mins <- paste('0',time1$min,sep='') else mins <- time1$min
	if(time1$hour<10) hours <- paste('0',time1$hour,sep='') else hours <- time1$hour
	
	if(is.na(secs)) secs <- '00'
	if(is.na(mins)) mins <- '00'
	if(is.na(hours)) hours <- '00'
	
	cat('\n   fitting model (started ',paste(hours,':',mins,':',secs,sep=''),')',sep='')
		
	return(time1)
}

displayFitEnd <- function(arfmodel,time1) {
	
	
	
	td=list(dif=0,un='none')
	
	time2 <- as.POSIXlt(Sys.time())
	diff.time <- difftime(time2,time1,units='sec')
	
	if(diff.time<60) {td$dif=diff.time;td$un='second(s)'}
	if(diff.time>=60 & diff.time<=3600) {td$dif=diff.time/60;td$un='minute(s)'}
	if(diff.time>3600) {td$dif=diff.time/3600;td$un='hours(s)'}
		
	cat(' finished in',round(td$dif,3),td$un,'\n\n',sep=' ')
	cat('  [model]\n')
	cat('   convergence:',.model.convergence(arfmodel),'\n\n')
		
}

displayEstimates <- function(arfmodel) {
	cat('                         theta1        theta2        theta3        theta4        theta5        theta6\n')
	cat('                   ----------------------------------------------------------------------------------\n')
	cat('   estimates ')
	cat(paste(' [ ',1,']',sep=''),sprintf('%12.6f ',.model.estimates(arfmodel)[((1+(1-1)*6):(1*6))]),'\n')
	if(.model.regions(arfmodel)>1) for(i in 2:.model.regions(arfmodel)) cat(paste('              [',sprintf('%2.0f',i),']',sep=''),sprintf('%12.6f ',.model.estimates(arfmodel)[((1+(i-1)*6):(i*6))]),'\n')
	cat('\n')
	
}

displayFit <- function(arfmodel) {
	
	cat('   model fit (BIC):',.model.fit(arfmodel),'\n\n')
	cat('   warnings:',.model.warnings(arfmodel),'\n\n')
	
}

displayStats <- function(arfmodel) {
	cat('                         theta1        theta2        theta3        theta4        theta5        theta6\n')
	cat('                   ----------------------------------------------------------------------------------\n')
	cat('   estimates ')
	cat(paste(' [ ',1,']',sep=''),sprintf('%12.6f ',.model.estimates(arfmodel)[((1+(1-1)*6):(1*6))]),'\n')
	if(.model.regions(arfmodel)>1) for(i in 2:.model.regions(arfmodel)) cat(paste('              [',sprintf('%2.0f',i),']',sep=''),sprintf('%12.6f ',.model.estimates(arfmodel)[((1+(i-1)*6):(i*6))]),'\n')
	cat('\n')
	cat('   s.e.      ')
	cat(paste(' [ ',1,']',sep=''),sprintf('%12.6f ',sqrt(diag(.model.varcov(arfmodel))[((1+(1-1)*6):(1*6))])),'\n')
	if(.model.regions(arfmodel)>1) for(i in 2:.model.regions(arfmodel)) cat(paste('              [',sprintf('%2.0f',i),']',sep=''),sprintf('%12.6f ',sqrt(diag(.model.varcov(arfmodel))[((1+(i-1)*6):(i*6))])),'\n')
	cat('\n\n')
	
	cat('                     x_location    y_location        extent     amplitude\n')
	cat('                   ------------------------------------------------------\n')
	cat('   stats     ')
	cat(paste(' [ ',1,']',sep=''),sprintf('%12.6f ',.wald.pvalues(.model.wald(arfmodel))[1,]),'\n')
	
	
	if(.model.regions(arfmodel)>1) for(i in 2:.model.regions(arfmodel)) cat(paste('              [',sprintf('%2.0f',i),']',sep=''),sprintf('%12.6f ',.wald.pvalues(.model.wald(arfmodel))[i,]),'\n')
	cat('\n')
	cat('\n')
}


displaySummary <- function(modelseq) {
	
	cat(' [ SEQUENCE ]')
	cat('\n\n')

	cat('  BIC trail:',round(.sequence.fit(modelseq)),paste('(',.sequence.minimum(modelseq),')',sep=''),'\n\n')
	
	if(length(.sequence.minimum(modelseq))==0) cat('  No valid model in sequence!\n\n') else {
		load(file=.sequence.mnames(modelseq)[which.min(.sequence.fit(modelseq))])
		
		cat('  [',.model.modelname(arfmodel),']\n\n')
				
		#displayEstimates(arfmodel)
		displayStats(arfmodel)
		displayFit(arfmodel)
	}
	
}

displayInit <- function(settings) {
	
	version=new('version')
	vstr=paste(.version.version(version),'.',.version.build(version),'-',.version.update(version),sep='')
	
	cat('Activated Region Fitting',vstr,'(',as.character(as.POSIXlt(Sys.time())),')\n')
	cat('\n')
		
}


displayDataModel <- function(arfmodel) {
		
	if(.model.valid(arfmodel)) {
		
		#open data for first trial to get dimensions and trial data
		trialdata <- readData(.model.avgdatfile(arfmodel))
		
		
		#set dimensions and data
		dimx <- .fmri.data.dims(trialdata)[2]
		dimy <- .fmri.data.dims(trialdata)[3]
		data <- .fmri.data.datavec(trialdata)[1:(dimx*dimy)]
		rm(trialdata)
		
		#calculate model based on parameter estimates
		model <- .C('gauss',as.double(.model.estimates(arfmodel)),as.integer(.model.regions(arfmodel)*6),as.integer(dimx),as.integer(dimy),as.double(numeric(dimx*dimy)))[[5]]
	
		browser()
		
		
		
	} else warning('No valid model. Residuals not calculated.')
	
	return(TRUE)
}
