#############################################
# arf3DS4 MAIN FUNCTIONS (initialize, arf)	#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

processModel <- function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) {
	
	#check if screen output is preferred
	if(!is.na(match('screen',.options.output.mode(options)))) pr=TRUE
	
	
	if(pr) {
		cat('[',.model.modelname(arfmodel),']\n')
		cat(' arf process for data',.model.name(arfmodel),'started',as.character(Sys.time()),'\n')
		cat(' fitting',.model.regions(arfmodel),'region(s)\n')
	}
	
	#fit the model
	arfmodel <- fitModel(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen)
	if(pr) cat(' ',.model.convergence(arfmodel),'\n',sep='')
	
	if(.model.valid(arfmodel)) {
		
		arfmodel = BIC(arfmodel,options=options)
		arfmodel = RMSEA(arfmodel,options=options)
		
		if(pr) cat(' modelfit:\n')
		if(pr) cat('  BIC  :',round(.model.fit(arfmodel)[1]),'\n')
		if(pr) cat('  RMSEA:',round(.model.fit(arfmodel)[2],1),'\n')
		
		if(pr) cat(' calculating derivatives...')
		md = makeDerivs(arfmodel)
		if(md) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		
		if(pr) cat(' calculating residuals...')
		rd = makeResiduals(arfmodel)
		if(rd) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		
		if(pr) cat(' calculating variance/covariance matrix...')
		arfmodel = varcov(arfmodel)
		if(.model.valid(arfmodel)) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		
		if(.model.valid(arfmodel)) {
			if(pr) cat(' calculating wald statistics...')
			arfmodel = wald(arfmodel)
			if(.model.valid(arfmodel)) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		} 
		
	} 
	
	#save the model
	saveModel(arfmodel)
	
	if(pr) cat(' modelfile saved in \'',.model.modelpath(arfmodel),.Platform$file.sep,.model.modelFile(arfmodel),'\'\n',sep='')
	if(pr) cat(' arf process stopped at',as.character(Sys.time()),'\n')
	
	return(invisible(arfmodel))
}