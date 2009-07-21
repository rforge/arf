#############################################
# arf3DS4 S4 ARFDATA FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


#listNoHdr lists files excluding .hdr files 
listNoHdr <- function(path,pattern='',full=T) 
{
	nameslist <-  list.files(path,pattern=pattern,full.names=full)
	whichhdr <- grep('.hdr',nameslist,value=F,ignore=T)
	
	if(length(whichhdr)>1) 
		return(nameslist[-whichhdr])
	else
		return(nameslist)
	
}

#load Rda data into a user-specified object (iso the named object that was saved) 
loadRda <- function(file) 
{
	
	#load file and save objectname in fn
	if(file.exists(file)) fn <- load(file) 
		else {
			fn <- ''
			warning('loadRda could not load file.')
		}
	
	#set object to be the eval of the loaded object
	object <- eval(parse(text=fn))	
	
	return(object)
}

#with global environment variable loaded, load Data based on subject and condition
loadData <- function(subject,condition,exp=.experiment)
{
	#set separator
	sp <- .Platform$file.sep
	
	#set filename based on subject and condition
	filename <- paste(.experiment.path(exp),sp,.experiment.subjectDir(exp),sp,.experiment.subjectPrefix(exp),subject,sp,.experiment.conditionDir(exp),sp,.experiment.conditionPrefix(exp),condition,sp,.experiment.dataDir(exp),sp,.experiment.dataRda(exp),sep='')
	
	return(invisible(loadRda(filename)))
}

#setDataObjects fills the DataObject within each subject/condition directory location of beta's weight's and averages.
setAllObjects <- function(experiment,overwrite=F)
{
	#set separator and allIsWell flag
	allIsWell=TRUE
	sp <- .Platform$file.sep
	
	#check for correctclass and valid experiment directories
	if(class(experiment)!='experiment') stop('Input must be of class \'experiment\'')
	if(!checkExp(experiment)) {warning('Experiment not valid. See warnings');allIsWell=F}
	
	#search within subjects and conditions directories
	for(subs in 1:.experiment.subject.num(experiment)) {
		for(conds in 1:.experiment.condition.num(experiment)) {
			
			#locate main path of datafiles structure
			path <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,.experiment.subject.names(experiment)[subs],sp,.experiment.conditionDir(experiment),sp,.experiment.condition.names(experiment)[conds],sep='')
						
			#create a new data file or read from the old one
			if(overwrite) {		
				data <- new('data')
				.data.name(data) <- paste(.experiment.subject.names(experiment)[subs],'-',.experiment.condition.names(experiment)[conds])
			} else {
				fn <- paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep='')
				
				if(file.exists(fn)) {
					data <- loadRda(paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep=''))
				} else {
					data <- new('data')
					.data.name(data) <- paste(.experiment.subject.names(experiment)[subs],'-',.experiment.condition.names(experiment)[conds])
				}
			}
			
			#set Fullpath
			.data.fullpath(data) <- path
			
			#set registration path and check if registration dirs are available
			.data.regDir(data) <- paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.regDir(experiment),sep='') 
			.data.regRda(data) <- .experiment.regRda(experiment)
			regIsWell <- checkRegs(data,overwrite=overwrite)
			
			#betafiles
			betafiles <- listNoHdr(paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.betaDir(experiment),sep=''),full=T)
			.data.betafiles(data) <- betafiles
			
			#weightfiles
			weightfiles <- listNoHdr(paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.weightsDir(experiment),sep=''),full=T)
			.data.weightfiles(data) <- weightfiles
			 
			#averagefiles
			.data.avgdatfile(data) <- listNoHdr(paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sep=''),.experiment.avgdatFile(experiment),full=T)
			.data.avgWfile(data) <- listNoHdr(paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sep=''),.experiment.avgWFile(experiment),full=T)
			
			#set number of trials (warn if beta/weight mismatch)
			if(length(betafiles)!=length(weightfiles)) {warning('Betafiles - Weightfiles mismatch!');allIsWell=F}
			.data.trials(data) <- length(betafiles)  		
						
			#checkFileIntegrity
			if(!checkFiles(data)) {warning('checkFiles returns false. Check warnings!');allIsWell=F}
			
			if(allIsWell) {
				#save the data
				save(data,file=paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep=''))
				
				#set dirs for all existing models
				modelpath <- paste(path,sp,.experiment.modelDir(experiment),sep='')
				modelname <- paste(modelpath,sp,.experiment.modelnamesRda(experiment),sep='')
			
				#updateModelNames 
				updateModelNames(modelname)
				mnames <- loadRda(modelname)
				
				if(length(mnames)>0) {
				#set correct dirs for each model
					for(mods in 1:length(mnames)) {
						model <- loadRda(paste(modelpath,sp,mnames[mods],sp,.experiment.modelRda(experiment),sep=''))
						.model.modelpath(model) <- paste(modelpath,sp,mnames[mods],sep='')
						.model.modeldatapath(model) <- paste(modelpath,sp,mnames[mods],sp,.experiment.modeldatDir(experiment),sep='')
						.model.fullpath(model) <- .data.fullpath(data)
						.model.betafiles(model) <- .data.betafiles(data)
						.model.weightfiles(model) <- .data.weightfiles(data)
						.model.avgdatfile(model) <- .data.avgdatfile(data)
						.model.avgWfile(model) <- .data.avgWfile(data)
						.model.regDir(model) <- .data.regDir(data)
						.model.regRda(model) <- .data.regRda(data)
						save(model,file=paste(modelpath,sp,mnames[mods],sp,.experiment.modelRda(experiment),sep=''))
						
					}
				}
			}
		}
	}
	
	return(invisible(allIsWell))
}



## checkFiles checks if the number of files and dimensions match
checkFiles <- function(arfdat) {
	
	if(class(arfdat)!='data') stop('Input must be of class \'data\'')
	
	#set allIsWell
	allIsWell=T
	
	#check if directory is valid and that there is at least one file in the directory 
	if(length(.data.betafiles(arfdat))<1 | length(.data.weightfiles(arfdat))<1) {
		warning(paste('No files in directory',.data.fullpath(arfdat)))
		allIsWell=F
	}
	
	#check if number of files matches
	if(length(.data.betafiles(arfdat))!=length(.data.weightfiles(arfdat))) {
		warning('Number of data and weight files do not match!')
		allIsWell=F
	}
	
	#check if dimensions of all files matches (first file in data dir is reference file)
	if(allIsWell) {
		filenames <- c(.data.betafiles(arfdat),.data.weightfiles(arfdat))
		
		headinforef <- getFileInfo(filenames[1])
		
		if(length(filenames)>1) {
			for(i in 2:length(filenames)) {
				headinfo <- getFileInfo(filenames[i])
				if(!identical(.nifti.header.dims(headinforef),.nifti.header.dims(headinfo))) {
					cat(.data.name(arfdat),'\n')
					warning('Dimensions of file ', .nifti.header.filename(headinfo),' do not match with reference file ',.nifti.header.filename(headinforef),'!')
					cat('Dimensions of',.nifti.header.filename(headinfo),':',.nifti.header.dims(headinfo),'\n')
					cat('Dimensions of',.nifti.header.filename(headinforef),':',.nifti.header.dims(headinforef),'\n')
					
					
					allIsWell=F
				}
			}
		}	
	}
	
	#return object of class data
	return(invisible(allIsWell))
	
}


#makeWeights creates uniform weight files for use with t-stat images
makeWeights <- function(experiment) {
	
	#set separator
	sp <- .Platform$file.sep
	
	#set initial directory
	subd <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sep='')
	
	#run through all dirs 
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		sn <- paste(subd,sp,.experiment.subjectPrefix(experiment),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.experiment.conditionDir(experiment),sep='')
				
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			cn <- paste(subc,sp,.experiment.conditionPrefix(experiment),.experiment.condition.names(experiment)[cdirs],sp,.experiment.dataDir(experiment),sp,.experiment.betaDir(experiment),sep='')
			nf <- paste(subc,sp,.experiment.conditionPrefix(experiment),.experiment.condition.names(experiment)[cdirs],sp,.experiment.dataDir(experiment),sp,.experiment.weightsDir(experiment),sep='')
			
			#list files in betadir
			tempfiles <- list.files(cn,full=T)
		
			#check if weightfiles exist
			weightfiles <- list.files(nf,full=T)
			
			#check if files already exist in the weightdir 
			if(length(tempfiles)!=length(weightfiles)) {
				if(length(weightfiles)==0) {
					
					#for each datafile create appropriate weightfiles
					for(fns in tempfiles) {
						
						tempdat <- readData(fns)
						.fmri.data.fullpath(tempdat) <- paste(nf,sp,sep='')
						.fmri.data.filename(tempdat) <- paste('weight_',.fmri.data.filename(tempdat),sep='')
						
						writeData(tempdat,rep(1,length(.fmri.data.datavec(tempdat))))
					}
				} else warning('some weightfiles already exist, no weights created.')
				
			} #if weightfiles exist do nothing
			
		}
	}
	
		
}

