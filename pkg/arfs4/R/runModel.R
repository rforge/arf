# ARF Run Model
# Wouter D. Weeda, University of Amsterdam
###############################################################################


runModel <- function(arfdata,modelseq) {
	
	for(model in .sequence.regions(modelseq)) {
		
		.sequence.current(modelseq) <- model
		
		arfmodel <- fitModel(arfdata,determineStart(readData(.data.avgdatfile(arfdata)),settings,model),model,settings)
		arfmodel <- BIC(arfmodel)
		arfmodel <- sandwich(arfmodel)
		#arfmodel <- wald(arfmodel,new('wald'))
		#model.valid(arfmodel)=TRUE
		filename <- paste(.data.fullpath(arfdata),'/stats/arfmodel_',.model.modelname(arfmodel),'.rda',sep='')
		.sequence.mnames(modelseq) <- c(.sequence.mnames(modelseq),filename)
		save(arfmodel,file=filename)
				
		if(.model.valid(arfmodel)) {
	
			displayEstimates(arfmodel)
			#displayStats(arfmodel)
					
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),.model.fit(arfmodel))
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),1)
			
		} else {
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),Inf)
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),0)
		}
	
		#displayFit(arfmodel)
		
		.sequence.minimum(modelseq) <- which.min(.sequence.fit(modelseq))
		if(!.sequence.valid(modelseq)[.sequence.minimum(modelseq)]) .sequence.minimum(modelseq)=integer(0)
	}
	
	
	if(length(.sequence.minimum(modelseq))==0) warning('No best model found.')
	
	#displaySummary(modelseq)
	
	save(modelseq,file=paste(.data.fullpath(arfdata),'/stats/last_sequence.rda',sep=''))
	
	return(invisible(modelseq))
}



runModelFast <- function(arfdata,modelseq) {
	
	for(model in .sequence.regions(modelseq)) {
		
		.sequence.current(modelseq) <- model
		
		arfmodel <- fitModel(arfdata,determineStart(readData(.data.avgdatfile(arfdata)),settings,model),model,settings)
		arfmodel <- BIC(arfmodel)
		
		filename <- paste(.data.fullpath(arfdata),'/stats/arfmodel_',.model.modelname(arfmodel),'.rda',sep='')
		.sequence.mnames(modelseq) <- c(.sequence.mnames(modelseq),filename)
		save(arfmodel,file=filename)
		
		if(.model.valid(arfmodel)) {
			
			displayEstimates(arfmodel)
			
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),.model.fit(arfmodel))
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),1)
			
		} else {
						
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),Inf)
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),0)
		}
		
		displayFit(arfmodel)
		
		
	}
	
	.sequence.minimum(modelseq) <- which.min(.sequence.fit(modelseq))
	
	filename <- .sequence.mnames(modelseq)[which.min(.sequence.fit(modelseq))]
	load(file=filename)
		
	arfmodel <- sandwich(arfmodel)
	arfmodel <- wald(arfmodel,new('wald'))

	save(arfmodel,file=filename)
		
	if(!.model.valid(arfmodel)) {
		.sequence.valid(modelseq)[.sequence.minimum(modelseq)]=0
	} 
	
	if(!.sequence.valid(modelseq)[.sequence.minimum(modelseq)]) .sequence.minimum(modelseq)=integer(0)	
		
	displaySummary(modelseq)
	
	save(modelseq,file=paste(.data.fullpath(arfdata),'/stats/last_sequence.rda',sep=''))
	
	return(invisible(modelseq))
		
	
}