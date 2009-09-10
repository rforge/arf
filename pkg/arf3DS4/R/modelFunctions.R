#############################################
# arf3DS4 S4 MODEL FUNCTIONS  	     		#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#newModel
#updateModelNames
#saveModel
#saveModelBin
#saveModelBinSimple
#loadOptions
#loadStart
#saveStart
#loadStart
#loadReg
#loadModel
#updateClass

newModel <- 
function(modelname='defaultmodel',regions=1,subject='',condition='',experiment=.experiment,options=new('options'),overwrite=T) 
#newModel makes a modeldirectory based on data and experiment information and a modelname
{
	#set separator
	sp <- .Platform$file.sep
	
	#load data from a condition
	if(subject=='') subject <- .experiment.subject.names(experiment)[1]
	if(condition=='') condition <- .experiment.condition.names(experiment)[1]
	arfdata <- loadRda(paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,subject,sp,.experiment.conditionDir(experiment),sp,condition,sp,.experiment.dataDir(experiment),sp,.experiment.dataRda(experiment),sep=''))
	
	#checks
	if(!class(arfdata)=='data') stop('Input must be of class \'data\'')
	if(modelname=='') stop('Modelname cannot be empty')
	
	
	#check if Averages exist (else create)
	if(!file.exists(.data.avgdatfile(arfdata)) | !file.exists(.data.avgWfile(arfdata)) | !file.exists(.data.avgtstatFile(arfdata))) {
		if(overwrite) {
			arfdata <- createAverages(arfdata,experiment)
		} else {
			warning('Averages do not exist, and overwrite is FALSE! Things might go wrong.')
		}
	} 
	
	#make new modelobject
	model <- new('model',arfdata)
		
	#set modelname
	.model.modelname(model) <- modelname 
			
	#path to modeldir
	path <- paste(.data.fullpath(arfdata),sp,.experiment.modelDir(experiment),sp,modelname,sep='')
	
	if(file.exists(path) & overwrite==F) stop('Model directory already exists')
	
	dir.create(path,show=F)
	.model.modelpath(model) <- path
	
	#path to modeldatadir
	dir.create(paste(path,sp,.experiment.modeldatDir(experiment),sep=''),show=F)
	.model.modeldatapath(model) <- paste(path,sp,.experiment.modeldatDir(experiment),sep='')
	
	#set filenames
	.model.residualFile(model) <- .experiment.residualFile(experiment)
	.model.derivativeFile(model) <- .experiment.derivativeFile(experiment)
	.model.weightFile(model) <- .experiment.weightFile(experiment)
	.model.modelFile(model) <- .experiment.modelRda(experiment)
	.model.optionsFile(model) <- .experiment.optionsRda(experiment)
	.model.modelDataFile(model) <- .experiment.modelDataFile(experiment)
	.model.startFile(model) <- .experiment.startRda(experiment)
	.model.logFile(model) <- .experiment.logFile(experiment)
		
	#set number of regions
	.model.regions(model) <- regions
	
	#save model and options File and startvec
	startval <- .model.startval(model) <- rep(.options.start.vector(options),.model.regions(model))
	save(startval,file=paste(path,sp,.model.startFile(model),sep=''))
	save(options,file=paste(path,sp,.model.optionsFile(model),sep=''))
	save(model,file=paste(path,sp,.model.modelFile(model),sep=''))
	
	
	#updateModelNames in on dir up
	updateModelNames(dirname(.model.modelpath(model)))
		
	return(model)
}


updateModelNames <- 
function(path) 
#update ModelNames in a ModelNamesFile
{
		
	#list all dirs in path (minus the modelnames file)
	existingfiles <- list.files(path,full=F)
	filename <- list.files(path,'.Rda',full=T)
	existingfiles <- existingfiles[-grep('.Rda',existingfiles)]

	#save modelnames
	save(existingfiles,file=filename)	
	
}

saveModel <- 
function(arfmodel) save(arfmodel,file=paste(.model.modelpath(arfmodel),.Platform$file.sep,.model.modelFile(arfmodel),sep=''))
#save the model to the model.Rda

saveModelBin <- 
function(arfmodel) 
#save the modelBinary
{
	
	#set separator
	sp <- .Platform$file.sep
	
	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	
	#set fullpaths
	.nifti.header.fullpath(headinf) <- .model.modeldatapath(arfmodel)
	.nifti.header.filename(headinf) <- .model.modelDataFile(arfmodel)
	.model.fullmodelDataFile(arfmodel) <- headToName(headinf)
	
	#write the Data to the modelNiftiFile
	writeData(headinf,.C('gauss',as.double(.model.estimates(arfmodel)),as.integer(.model.regions(arfmodel)*.model.params(arfmodel)),as.integer(.nifti.header.dims(headinf)[2]),as.integer(.nifti.header.dims(headinf)[3]),as.integer(.nifti.header.dims(headinf)[4]),as.double(numeric(.nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4])))[[6]])
	
	return(arfmodel)
	
}

saveModelBinSimple <- 
function(arfmodel) 
#save the modelBinary
{
	
	#set separator
	sp <- .Platform$file.sep

	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))

	#set fullpaths
	.nifti.header.fullpath(headinf) <- .model.modeldatapath(arfmodel)
	.nifti.header.filename(headinf) <- .model.modelDataFile(arfmodel)
	.model.fullmodelDataFile(arfmodel) <- headToName(headinf)
	
	#write the Data to the modelNiftiFile
	writeData(headinf,.C('simplegauss',as.double(.model.estimates(arfmodel)),as.integer(.model.regions(arfmodel)*.model.params(arfmodel)),as.integer(.nifti.header.dims(headinf)[2]),as.integer(.nifti.header.dims(headinf)[3]),as.integer(.nifti.header.dims(headinf)[4]),as.double(numeric(.nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4])))[[6]])
	
	return(arfmodel)
	
}


loadOptions <- 
function(arfmodel) return(loadRda(paste(.model.modelpath(arfmodel),.Platform$file.sep,.model.optionsFile(arfmodel),sep='')))
#loadOptions loads the options object

loadStart <- 
function(arfmodel) return(loadRda(paste(.model.modelpath(arfmodel),.Platform$file.sep,.model.startFile(arfmodel),sep='')))
#loadStart loads the Start object

saveStart <- 
function(startval,arfmodel) save(startval,file=paste(.model.modelpath(arfmodel),.Platform$file.sep,.model.startFile(arfmodel),sep=''))  	
#save startingvalues

loadReg <- 
function(arfmodel) return(loadRda(.model.regfile(arfmodel),sep=''))
#loadReg

loadModel <- 
function(modelname,subject,condition,experiment=.experiment) 
#load a model based on subject and conditions
{
	sp <- .Platform$file.sep
	modname = paste(.experiment.path(experiment),sp,.experiment.subjectDir(experiment),sp,subject,sp,.experiment.conditionDir(experiment),sp,condition,sp,.experiment.modelDir(experiment),sp,modelname,sp,.experiment.modelRda(experiment),sep='')
	mod = loadRda(modname)
	
	return(mod)
}

updateClass <- 
function(object,...) 
#update a modelclass with elements
{

	new_object <- new(class(object),object,...)
	return(new_object)
	
}

