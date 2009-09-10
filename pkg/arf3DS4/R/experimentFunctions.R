#############################################
# arf3DS4 S4 EXPERIMENT FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#checkExp
#makeExpDirs
#chngRootExp
#setExp
#loadExp
#getFSL
#checkVersion

checkExp <- 
function(experiment) 
# checkExp determines if the experiment structure is valid
{
	
	#set separator and 'all is well' flag
	sp <- .Platform$file.sep
	allIsWell=TRUE
	
	#check if input is of class 'experiment'
	if(class(experiment)!='experiment') stop('Input must be of class \'experiment\'')
	
	subd <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sep='')
	
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		sn <- paste(subd,sp,.experiment.subjectPrefix(experiment),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.experiment.conditionDir(experiment),sep='')
		
		for(cdirs in 1:.experiment.condition.num(experiment )) {
			
			cn <- paste(subc,sp,.experiment.conditionPrefix(experiment),.experiment.condition.names(experiment)[cdirs],sep='')
			
			#check if data,weights,avg and models directories exist.
			if(!file.exists(paste(cn,sp,.experiment.modelDir(experiment),sep=''))) {warning(paste('In:',cn,' modelDir does not exits or does not match settings',sep=''));allIsWell=F}
			if(!file.exists(paste(cn,sp,.experiment.dataDir(experiment),sep=''))) {warning(paste('In:',cn,' dataDir does not exits or does not match settings',sep=''));allIsWell=F}
			if(!file.exists(paste(cn,sp,.experiment.dataDir(experiment),sp,.experiment.betaDir(experiment),sep=''))) {warning(paste('In:',cn,sp,.experiment.dataDir(experiment),' betaDir does not exits or does not match settings',sep=''));allIsWell=F}
			if(!file.exists(paste(cn,sp,.experiment.dataDir(experiment),sp,.experiment.weightsDir(experiment),sep=''))) {warning(paste('In:',cn,sp,.experiment.dataDir(experiment),' weightsDir does not exits or does not match settings',sep=''));allIsWell=F}
			if(!file.exists(paste(cn,sp,.experiment.dataDir(experiment),sp,.experiment.avgDir(experiment),sep=''))) {warning(paste('In:',cn,sp,.experiment.dataDir(experiment),' avgDir does not exits or does not match settings',sep=''));allIsWell=F}
			
		}
	}
	
	#return FALSE if not correct, return TRUE if all is well
	return(invisible(allIsWell))
}


makeExpDirs <- 
function(path=getwd(),name='default_experiment',subjectind=1,conditionind=1,settings=new('settings'))
# makeExpDirs creates a directory structure given the path, number of subjects, conditions and a settings object
# by default creates a structure in the working directory with 1 subject 1 condition and default settings
{
	#set separator
	sp <- .Platform$file.sep
	
	#check if path exists
	if(path=='') path=paste(getwd(),sp,name,sep='') else path=paste(path,sp,name,sep='')
	if(!file.exists(path)) dir.create(path,recursive=T)
	
	#set path separator at end
	path <- file.path(dirname(path),basename(path),'')
	
	#create new experimentclass
	experiment <- new('experiment',settings)
	
	#set the experiment values
	.experiment.path(experiment)=path
	.experiment.name(experiment)=name
	.experiment.subject.num(experiment)=length(subjectind)
	.experiment.subject.names(experiment)=as.character(subjectind)
	.experiment.condition.num(experiment)=length(conditionind)
	.experiment.condition.names(experiment)=as.character(conditionind)
		
	#save experiment Rda
	save(experiment,file=paste(.experiment.path(experiment),sp,.settings.expRda(settings),sep=''))
	
	#create subjects directory
	subd <- paste(path,sp,.settings.subjectDir(settings),sep='')
	dir.create(subd,show=F)
	
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		#create individual subject dirs
		sn <- paste(subd,sp,.settings.subjectPrefix(settings),.experiment.subject.names(experiment)[sdirs],sep='')
		dir.create(sn,show=F)
		
		#create conditions dir
		subc <- paste(sn,sp,.settings.conditionDir(settings),sep='')
		dir.create(subc,show=F)		

		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			#create individual condition dirs
			cn <- paste(subc,sp,.settings.conditionPrefix(settings),.experiment.condition.names(experiment)[cdirs],sep='')
			dir.create(cn,show=F)
			
			#models dir and modelnamesfile
			dir.create(paste(cn,sp,.settings.modelDir(settings),sep=''),show=F)
			mnames=''
			save(mnames,file=paste(cn,sp,.settings.modelDir(settings),sp,.settings.modelnamesRda(settings),sep=''),show=T)
			
			#data,weights,avg
			dn <- paste(cn,sp,.settings.dataDir(settings),sep='')
			dir.create(dn,show=F)
						
			dir.create(paste(dn,sp,.settings.betaDir(settings),sep=''),show=F)
			dir.create(paste(dn,sp,.settings.weightsDir(settings),sep=''),show=F)
			dir.create(paste(dn,sp,.settings.avgDir(settings),sep=''),show=F)
			dir.create(paste(dn,sp,.settings.regDir(settings),sep=''),show=F)
			dir.create(paste(dn,sp,.settings.funcDir(settings),sep=''),show=F)
			
			
		
		}
	}
	
	#save experiment
	save(experiment,file=paste(.experiment.path(experiment),sp,.experiment.expRda(experiment),sep=''))

	#final check
	if(!checkExp(experiment)) {
		warning('Experiment did not pass checks! Check warnings!')
	} else return(invisible(experiment))
	
}


chngRootExp <- 
function(path=getwd(),quiet=F) 
# resets and checks the experiment path
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#check if one file is in the experiment root
	if(length(list.files(path,'.Rda',full=T))!=1) stop('No experiment rda file found or multiple rda files found.')
	experiment <- loadRda(list.files(path,'.Rda',full=T))
	
	#set the correct path
	path <- path.expand(path)
	path <- file.path(dirname(path),basename(path),'')
	
	.experiment.path(experiment) <- path
	
	#check the experiment dirs, if good save and exit. if not good stop
	if(checkExp(experiment)) {
		save(experiment,file=paste(.experiment.path(experiment),sp,.experiment.expRda(experiment),sep=''))
		if(!quiet) cat('Experiment path changed. Experiment saved to',paste(.experiment.path(experiment),sp,.experiment.expRda(experiment),sep=''),'\n')
	} else {
		stop('Experiment structure not valid,check warnings.')
	}
	
	return(invisible(experiment))
}


setExp <- 
function(path=getwd(),tempsub=1,tempcond=1,auto=TRUE,createWeights=TRUE,overwrite=F,load=T) 
#setExp creates an experiment-class object based on existing directories
{
	#set separator
	sp <- .Platform$file.sep
	
	#check if only dir else pre-append working directory
	if(length(grep(sp,path))==0) path <- paste(getwd(),sp,path,sep='')
	
	#set path separator at end
	path <- file.path(dirname(path),basename(path),'')
	
	#set expname
	expname <- strsplit(path,.Platform$file.sep)[[1]]
	expname <- expname[length(expname)]
	
	#if experiment file exists open it, else create a new one
	exp <-  list.files(path,pattern='Rda',full=T)
	if(length(exp)==1 & overwrite==FALSE) settings <- loadRda(exp) else settings <- new('settings') 
		
	#create new experimentclass
	experiment <- new('experiment',settings)
	.experiment.name(experiment) <- expname
	.experiment.path(experiment) <- path
	
	#cat('[',toupper(expname),']\n')
	#cat(' Experiment root:',path,'\n')
	
	#determine directories subjects
	whichdirs <- file.info(list.files(.experiment.path(experiment),full=T))$isdir
	fileList <- list.files(.experiment.path(experiment),full=F)[whichdirs]
	if(length(fileList)!=1) stop('Multiple subject directories found in rootdir') 
		
	#set subjectsdirname
	.experiment.subjectDir(experiment) <- fileList
	#cat('  \\Subjects directory:',fileList,'\n')
	
	#get subjects data
	subd <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sep='')
	whichdirs <- file.info(list.files(subd,full=T))$isdir
	fileList <- list.files(subd,full=F)[whichdirs]
	.experiment.subject.num(experiment) <- length(fileList)
	.experiment.subject.names(experiment) <- fileList
	
	#go to the first subject		
	sn <- paste(subd,sp,.experiment.subjectPrefix(experiment),.experiment.subject.names(experiment)[tempsub],sep='')
	whichdirs <- file.info(list.files(sn,full=T))$isdir
	fileList <- list.files(sn,full=F)[whichdirs]
	
	#get conditions dirname
	if(length(fileList)!=1) stop('Multiple condition directories found') 
	.experiment.conditionDir(experiment) <- fileList
	#cat('   \\Condition directory:',fileList,'\n')
			
	#get condition data
	subc <- paste(sn,sp,.experiment.conditionDir(experiment),sep='')
	whichdirs <- file.info(list.files(subc,full=T))$isdir
	fileList <- list.files(subc,full=F)[whichdirs]
	.experiment.condition.num(experiment) <- length(fileList)
	.experiment.condition.names(experiment) <- fileList
	
	#go to first condition 	
	cn <- paste(subc,sp,.experiment.conditionPrefix(experiment),.experiment.condition.names(experiment)[tempcond],sep='')
	whichdirs <- file.info(list.files(cn,full=T))$isdir
	fileList <- list.files(cn,full=F)[whichdirs]
		
	if(!auto) { 	
		#set the data and model directories
		cat('    the following directories have been found.\n')
		wdir <- numeric(length(fileList))
		if(length(fileList)<2) stop('Not enough directories found!')
		names(wdir) <- c('data','models')
		
		for(i in 1:length(fileList)) cat('     [',i,'] ',fileList[i],'\n',sep='')
		for(i in 1:2) wdir[i] <- as.numeric(readline(paste('    Please indicate the number corresponding to the',names(wdir)[i],'directory: ')))
				
		.experiment.dataDir(experiment) <- fileList[wdir[1]]
		.experiment.modelDir(experiment) <- fileList[wdir[2]]
		
		#goto dataDir
		cn <- paste(subc,sp,.experiment.conditionPrefix(experiment),.experiment.condition.names(experiment)[tempcond],sp,.experiment.dataDir(experiment),sep='')
		whichdirs <- file.info(list.files(cn,full=T))$isdir
		fileList <- list.files(cn,full=F)[whichdirs]
		
		cat('    the following directories have been found within data.\n')
		wdir <- numeric(length(fileList))
		if(length(fileList)<5) stop('Not enough directories found!')
		names(wdir) <- c('average','beta','func','reg','weights')
		
		
		for(i in 1:length(fileList)) cat('     [',i,'] ',fileList[i],'\n',sep='')
		for(i in 1:5) wdir[i] <- as.numeric(readline(paste('    Please indicate the number corresponding to the',names(wdir)[i],'directory: ')))
		
		.experiment.avgDir(experiment) <- fileList[wdir[1]]
		.experiment.betaDir(experiment) <- fileList[wdir[2]]
		.experiment.funcDir(experiment) <- fileList[wdir[3]]
		.experiment.regDir(experiment) <- fileList[wdir[3]]
		.experiment.weightsDir(experiment) <- fileList[wdir[4]]
		
	
	} 
	
	#make uniform weights when no weights exist
	if(createWeights) makeWeights(experiment)
	
	#setAllObjects
	setAllObjects(experiment,overwrite)
	
	#check the experiment dirs, if good save and exit. if not good stop
	if(checkExp(experiment)) {
		save(experiment,file=paste(.experiment.path(experiment),.settings.expRda(settings),sep=''))
		cat('Experiment correctly set. Experiment saved to',paste(.experiment.path(experiment),.settings.expRda(settings),sep=''),'\n')
		if(load) loadExp(paste(.experiment.path(experiment),sep=''))
		
		.experiment <- experiment
		save('.experiment',file=paste(.experiment.path(experiment),.Platform$file.sep,'temp.Rda',sep=''))
		load(paste(.experiment.path(experiment),.Platform$file.sep,'temp.Rda',sep=''),envir=.GlobalEnv)
		file.remove(paste(.experiment.path(experiment),.Platform$file.sep,'temp.Rda',sep=''))

	} else {
		stop('Experiment structure not valid,check warnings.')
	}
	
	return(invisible(experiment))
	
}


loadExp <- 
function(path=getwd(),method=c('fast','set','rda'))
#loadExp loads an experiment 
{
	
	method = match.arg(method)
	
	if(method=='fast') fast=TRUE
	if(method=='set') {
		fast=FALSE
		overwrite=TRUE
		set=TRUE
	}
	if(method=='rda') {
		fast=FALSE
		overwrite=TRUE
		set=FALSE
	}
	#set separator
	sp <- .Platform$file.sep
	
	allIsWell = TRUE
	
	#check if only dir else pre-append working directory
	if(length(grep(sp,path))==0) path <- paste(getwd(),sp,path,sep='')
	
	#set filename and load experiment
	filename <- list.files(path,'.Rda',full=T)
	if(length(filename)!=1) stop('No experiment rda file found or multiple rda files found.')
	.experiment <- experiment <- loadRda(filename)
	
	if(!fast) {
		#remove filename to obtain root-path
		fn <- strsplit(filename,.Platform$file.sep)[[1]]
		path <- sub(fn[length(fn)],'',filename)
		path <- path.expand(path)
		path <- file.path(dirname(path),basename(path),'')
		
		if(!set)  {
			#change root for the experiment-file to the current root
			.experiment <- experiment <- chngRootExp(path,quiet=T)
			#set and check all objects based on subjects/condition info and settings
			allIsWell <- setAllObjects(experiment,over=overwrite) 
		} else .experiment <- experiment <- setExp(path,1,1,TRUE,TRUE,TRUE,FALSE)
		
	}  
	
	#save experiments
	save('.experiment',file=paste(path,.Platform$file.sep,'temp.Rda',sep=''))
	load(paste(path,.Platform$file.sep,'temp.Rda',sep=''),envir=.GlobalEnv)
	file.remove(paste(path,.Platform$file.sep,'temp.Rda',sep=''))
	save(experiment,file=paste(.experiment.path(experiment),sp,.experiment.expRda(experiment),sep=''))
		
	#return loaded info 
	if(allIsWell) cat('Loaded experiment ',.experiment.name(experiment),' (version ',.version.version(.experiment.version(experiment)),'.',.version.build(.experiment.version(experiment)),'-',.version.update(.experiment.version(experiment)),')\n',sep='') else cat('Loaded experiment ',.experiment.name(experiment),' (version',.version.version(.experiment.version(experiment)),'.',.version.update(.experiment.version(experiment)),'-',.version.build(.experiment.version(experiment)),') with warnings!\n',sep='')
	
	return(invisible(experiment))
		
}



getFSL <- 
function(experiment,subject,condition,featpath,subjectname,contrastnum) 
#get FSL data from a FEAT directory
{
	
	sp=.Platform$file.sep
	
	cat('Subject:',subjectname,'\n')
		
	fns <- list.files(path=featpath,pattern=subjectname)
	
	rn=1
	for(run in fns) {
		
		filename <- paste(featpath,sp,run,sp,'stats',sp,'tstat',contrastnum,'.nii.gz',sep='')
		
		
		path <- paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,subject,sp,.experiment.conditionDir(experiment),sp,condition,sep='')
		newfilename <- paste(path,sp,.experiment.dataDir(experiment),sp,.experiment.betaDir(experiment),sp,rn,'tstat',contrastnum,'.nii.gz',sep='')
		
		cat(' ',filename,'->',newfilename,'\n')
		file.copy(filename,newfilename,overwrite=T)
				
		rn=rn+1
	}
	
}


checkVersion <- 
function(curversion,version=1,build=0,update=0) 
#checks the version of the ARF code (check if curversion larger than given version)
{
	allIsWell=FALSE
		
	if(version < .version.version(curversion)) {
		allIsWell=TRUE
	} else {
		if(version == .version.version(curversion)) {
			if(build < .version.build(curversion)) {
				allIsWell=TRUE
			} else {
				if(build == .version.build(curversion)) {
					if(update < .version.update(curversion)) {
						allIsWell=TRUE
					}						
				}
			}	
		}
	}
	
	return(allIsWell)
}
