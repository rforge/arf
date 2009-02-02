# MAIN FUNCTIONS (initialize, ARF)
# Wouter D. Weeda, University of Amsterdam
###############################################################################


initialize <- function(path='',expname=NULL,fullSummary=T,createAverages=T) {
	
	if(!exists('settings',where=.GlobalEnv)) assign('settings',new('settings'),envir=.GlobalEnv)
	
	displayInit(settings)
	
	if(!file.exists(path)) stop('Invalid Path')
		
	arfdata <- makeDataClass(path)
	
	if(createAverages) arfdata <- createAverages(arfdata)
	
	if(!file.exists(paste(path,'/stats',sep=''))) dir.create(paste(path,'/stats',sep=''))
	
	if(!is.null(expname)) .data.name(arfdata) <- as.character(expname)
		
	if(fullSummary) {
		cat('[global settings]\n')
		cat('  iteration limit:',.settings.min.iterlim(settings),'\n')
		cat('  SV method:',.settings.start.method(settings),'\n')
		cat('  SV fwhm:',.settings.start.maxfac(settings),'\n')
		cat('  SV values:',.settings.start.vector(settings),'\n')
		cat('  RCK method:',.settings.chk.method(settings),'\n')
		cat('  RCK ranges:',.settings.chk.range(settings),'\n')
		cat('\n')
				
		headinf <- readHeader(getFileInfo(.data.avgdatfile(arfdata)))
		
		cat(paste('[ *** ',toupper(.data.name(arfdata)),' *** ]\n',sep=''))
		cat('\n')
		cat(' dimensions:',.nifti.header.dims(headinf)[2],'x',.nifti.header.dims(headinf)[3],'\n')
		cat(' datatype:',.nifti.header.data.type(headinf),'\n')
		cat(' storagetype:',.nifti.header.filetype(headinf),'\n')
		cat(' path:',.data.fullpath(arfdata),'\n')
		cat(' trials:',.data.trials(arfdata),' ')
		if(createAverages) cat('(averages calculated)\n') 
		else {
			if(length(list.files(paste(.data.fullpath(arfdata),'avg/',sep='')))==2) cat('(averages exist)\n') 
			else {
				cat('\n')
				stop('No averages exist and createAverages=FALSE')
			}
		}
	
	} else {
		cat(paste('[',toupper(.data.name(arfdata)),']\n',sep=''))
	}

	return(invisible(arfdata))
}




arf <- function(path='',expname=NULL,sequence=c('manual','sequential','interleaved'),regionstofit=1,fast=F,fullOutput=T,createAverages=T) {
	
	arfdata <- initialize(path,expname=expname,fullSummary=fullOutput,createAverages=T)
	
	modelseq <-	new('sequence')
		
	if(match.arg(sequence)=='sequential') .sequence.regions(modelseq) <- seq(1,as.numeric(regionstofit))
	if(match.arg(sequence)=='interleaved') .sequence.regions(modelseq) <- seq(1,as.numeric(regionstofit),by=2)
	if(match.arg(sequence)=='manual') .sequence.regions(modelseq) <- as.numeric(regionstofit)
	
	cat(' sequence:',match.arg(sequence))
	if(fast) cat(' (fast mode)\n') else cat(' (normal mode)\n')
	
	cat(' regions to fit (',length(.sequence.regions(modelseq)),') ->',.sequence.regions(modelseq),'\n')
	cat('\n')
	
	if(fast) modelseq <- runModelFast(arfdata,modelseq) else modelseq <- runModel(arfdata,modelseq)
	
	
	if(fullOutput) {
		cat(' sequence info in:',paste(.data.fullpath(arfdata),'/stats/last_sequence.rda',sep=''),'\n')
		if(length(.sequence.minimum(modelseq))!=0) cat(' best model in:',.sequence.mnames(modelseq)[which.min(.sequence.fit(modelseq))],'\n\n') 
	}

	cat('[ ARF PROCEDURE FINISHED ] -------------------------------------\n')
	
}
