#############################################
# arf3DS4 MAIN FUNCTIONS                 	#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#processModel

processModel <- 
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),pr=T,printlevel=0,try.silen=T) 
#processModel does an entire arf-fitting pipeline 
{

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


processSequence <-
function(modelname='defaultmodel',seedreg=10,subject='',condition='',options=new('options'),overwrite=T,pr=T,printlevel=0,try.silen=T,experiment=NULL)
#process a sequence based on a seed number of regions
{
	
	if(pr) cat('[',modelname,'] @ seed',seedreg,'- started',as.character(Sys.time()),'\n')
	#run a simple model
	.options.start.method(options) = 'rect'
	simple_model = newModel(paste('simple_',modelname,sep=''),regions=seedreg,subject=subject,condition=condition,type='simple',options=options,overwrite=overwrite,experiment=experiment)
	
	if(pr) cat(as.character(Sys.time()),'fitting simple model...')
	simple_model = fitModel(simple_model)
	if(pr) cat('ok\n')
	
	if(.model.valid(simple_model)) .options.start.method(options) = 'use' else .options.start.method(options) = 'rect'
	
	#run a full model (with informed starts)
	full_model = newModel(paste('full_',modelname,sep=''),regions=seedreg,subject=subject,condition=condition,type='gauss',options=options,overwrite=overwrite,experiment=experiment)
	
	if(.model.valid(simple_model)) {
		.model.startval(full_model) = .model.estimates(simple_model)
		.model.startval(full_model)[.model.startval(full_model)==0]=.01
		saveModel(full_model)
		if(pr) cat(as.character(Sys.time()),'fitting full model...')
		full_model = fitModel(full_model)
		if(pr) cat('ok\n')
	} else model = NULL #no valid models 
	
	#run prune sequence
	#pruned model
	if(.model.valid(full_model)) {
	
		stop_prune = FALSE
		prune_num = 1
		
		pruned_model = full_model
		
		while(stop_prune==FALSE) 
		{
			if(pr) cat(as.character(Sys.time()),'fitting pruned',prune_num,'model.')
			ests = matrix(.model.estimates(pruned_model),10)
			
			b_del = checkSolutionReturn(pruned_model)
			g_del = checkGradientReturn(pruned_model)
			del = unique(c(b_del,g_del))
			
			if(length(del)>0) {
				ests = ests[,-del]
				.options.start.method(options) = 'use'
				pruned_model = newModel(paste('pruned',prune_num,'_',modelname,sep=''),regions=ncol(ests),subject=subject,condition=condition,type='gauss',options=options,overwrite=overwrite,experiment=experiment)
				.model.startval(pruned_model) = as.vector(ests)
				saveModel(pruned_model)
				pruned_model = processModel(pruned_model,pr=pr)
				
			} else {
				cat('no pruning required!\n')
				model = full_model #pruned_model not valid
				stop_prune = TRUE
			}
			
			prune_num = prune_num + 1
			
		} #end prune while
	
		model = pruned_model
		
	} else 	model = simple_model #full_model not valid
	
	
	return(model)
	
}


