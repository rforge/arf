`runModel` <-
function(directory,sequence=c('sequential','manual'),regionstofit=1) {
	
	if(!file.exists(paste(directory,'/stats',sep=''))) dir.create(paste(directory,'/stats',sep=''))

	modelseq <-	new('sequence')
	
	if(sequence[1]=='sequential') .sequence.regions(modelseq) <- seq(1,regionstofit)
	if(sequence[1]=='manual') .sequence.regions(modelseq) <- regionstofit
	
	arfdata <- makeDataClass(directory)
	arfdata <- createAverages(arfdata)
	
	for(model in .sequence.regions(modelseq)) {
		
		.sequence.current(modelseq) <- model
		
		arfmodel <- fitModel(arfdata,determineStart(readData(.data.avgdatfile(arfdata)),settings,model),model,settings)
		arfmodel <- BIC(arfmodel)
		arfmodel <- sandwich(arfmodel)
		arfmodel <- wald(arfmodel,new('wald'))
	
		filename <- paste(directory,'/stats/arfmodel_',.model.modelname(arfmodel),'.rda',sep='')
		.sequence.mnames(modelseq) <- c(.sequence.mnames(modelseq),filename)
		save(arfmodel,file=filename)
				
		if(.model.valid(arfmodel)) {
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),.model.fit(arfmodel))
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),1)
			
		} else {
			.sequence.fit(modelseq) <- c(.sequence.fit(modelseq),Inf)
			.sequence.valid(modelseq) <- c(.sequence.valid(modelseq),0)
		}
	
		if(length(.sequence.fit(modelseq))>0) .sequence.minimum(modelseq) <- which.min(.sequence.fit(modelseq))
	}
	
	if(.sequence.minimum(modelseq)==0) warning('No best model found.')
	
	
	save(modelseq,file=paste(directory,'/stats/last_sequence.rda',sep=''))
	
	return(invisible(modelseq))
}

