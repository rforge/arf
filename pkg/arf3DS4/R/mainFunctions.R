#############################################
# arf3DS4 MAIN FUNCTIONS                 	#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#processModel

processModel <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) 
#processModel does an entire arf-fitting pipeline 
{
	
	#check if screen output is preferred
	if(!is.na(match('screen',.options.output.mode(options)))) pr=TRUE
	
	.model.warnings(arfmodel) <- character(0)
	.model.convergence(arfmodel) <- character(0)
	
				
	if(pr) {
		cat('[',.model.modelname(arfmodel),']\n')
		cat(' arf process for data',.model.name(arfmodel),'started',as.character(Sys.time()),'\n')
		cat(' fitting',.model.regions(arfmodel),'region(s)\n')
	}
	
	arfmodel <- fitModel(arfmodel,options=options,dat=dat,weights=weights,printlevel=printlevel,try.silen=try.silen)
	
	if(pr) cat(' ',.model.convergence(arfmodel),'\n',sep='')
	
	if(.model.valid(arfmodel)) {
		
		if(pr) cat(' <modelfit>\n')
		if(pr) cat('  minimum:',round(.model.minimum(arfmodel)),'\n')
		if(pr) cat('    BIC  :',round(.model.fit(arfmodel)[1]),'\n')
		if(pr) cat('    RMSEA:',round(.model.fit(arfmodel)[2],1),'\n')
		
		if(pr) cat('\n calculating variance/covariance matrix...')
		arfmodel = varcov(arfmodel)
		if(.model.valid(arfmodel)) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		
		if(.model.valid(arfmodel)) {
			if(pr) cat(' calculating wald statistics...')
			arfmodel = wald(arfmodel)
			if(.model.valid(arfmodel)) {if(pr) {cat('ok\n')}} else {if(pr) {cat('fail\n')}} 
		} else {
			if(pr) cat(' wald statistics not calculated.\n')
			
		}
	
	} 
	
	#save the model
	saveModel(arfmodel)
	
	if(pr) show(arfmodel)
	
	if(pr) cat(' \nmodelfile saved in \'',.model.modelpath(arfmodel),.Platform$file.sep,.model.modelFile(arfmodel),'\'\n',sep='')
	if(pr) cat(' arf process stopped at',as.character(Sys.time()),'\n\n')
	
	return(invisible(arfmodel))
}