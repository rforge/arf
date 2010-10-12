#############################################
# arf3DS4 S4 ADDITIONAL DATA				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#arf2Excel

makeExample <- 
function(path) {
	
	if(missing(path)) path=system.file('data',package='arf3DS4')
	
	data('arf-full-data')
	example = .example
	
	#Definitions
	expname = 'example-experiment-fulldata'
	subject = 'wouter'
	condition = 'A'
	p = .Platform$file.sep
			
	#makeExperimentDirs
	makeExpDirs(paste(path,p,sep=''),expname,subject,condition)
	
	#write Nifti objects to path
	fullpath = paste(path,p,expname,p,'subjects',p,subject,p,'conditions',p,condition,p,'data',p,'beta',sep='')
	example$tstat1@fullpath = example$tstat2@fullpath = fullpath
	writeData(example$tstat1)
	writeData(example$tstat2)
	
	fullpath = paste(path,p,expname,p,'subjects',p,subject,p,'conditions',p,condition,p,'data',p,'funcs',sep='')
	example$sevents@fullpath = fullpath
	writeData(example$sevents)
	
	fullpath =  paste(path,p,expname,p,'subjects',p,subject,p,'funcs',sep='')
	example$rawdata@fullpath = fullpath
	writeData(example$rawdata)
	
	#load and set experiment
	ex = loadExp(paste(path,p,expname,sep=''),'set')
	createAllAverages(ex)
	
	#assign .experiment to the .GlobalEnv  
	assign('.experiment',ex,envir=.GlobalEnv)
	
	return(invisible(ex))
	
}

