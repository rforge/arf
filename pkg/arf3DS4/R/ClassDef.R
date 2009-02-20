#############################################
# arf3DS4 S4 CLASS DEFINITIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################


## arf version class (version is set here)
setClass(
		Class='version',
		representation=representation(
			version='numeric',
			build='numeric',
			update='numeric'
		),
		prototype=prototype(
			version=1,
			build=1,
			update=3
		)
)


## arf settings class (containing all standard directory names)
setClass(
		Class='settings',
		representation=representation(
			expRda='character',				#name of experimentobject
			optionsRda='character',			#name of optionsobject
			startRda='character',			#name of startvec file
			dataRda='character',			#name of dataobject in each datadir
			modelRda='character',			#name of modeldatafile
			statsRda='character',			#name of statsfile
			subjectPrefix='character', 		#subjects prefix
			conditionPrefix='character', 	#condition prefix
			modelPrefix='character',		#model prefix
			subjectDir='character', 		#subjects prefix
			conditionDir='character', 		#condition prefix
			dataDir='character',			#data directory name
			weightsDir='character',			#weights directory name
			avgDir='character',				#avg directory name
			betaDir='character',			#beta directory
			modelDir='character',			#model directory
			statsDir='character',			#stats directory
			modeldatDir='character', 		#Residual/Derivatives/Weights directory
			avgdatFile='character',			#averageDataFileName
			avgWFile='character',			#averageWeightFileName
			modelDataFile='character',		#name of modelDataiFile
			modelnamesRda='character',		#modelNames file
			residualFile='character',		#name of residual binary
			derivativeFile='character',		#name of derivative binary
			weightFile='character'
			
		),
		prototype=prototype(
			expRda='experiment.Rda',		#name of experimentobject
			optionsRda='options.Rda',		#name of optionsobject
			startRda='start.Rda',			#startvec
			dataRda='data.Rda',				#name of dataobject in each data
			modelRda='model.Rda',			#name of modelfile
			statsRda='stats.Rda',			#name of statsfile
			subjectPrefix='', 				#subjects prefix
			conditionPrefix='', 			#condition prefix
			subjectDir='subjects', 			#subjects prefix
			conditionDir='conditions', 		#condition prefix
			modelPrefix='model',			#model prefix
			dataDir='data',					#data directory name
			weightsDir='weights',			#weights directory name
			avgDir='avg',					#avg directory name
			betaDir='beta',					#beta directory
			modelDir='models',				#model directory
			statsDir='stats',				#stats directory
			modeldatDir='data', 			#Residual/Derivatives/Weights directory	
			avgdatFile='avgdata',			#averageDataFileName
			avgWFile='avgweight',			#averageWeightFileName
			modelDataFile='avgmodel',		#name of modelNiftiFile
			modelnamesRda='modelnames.Rda', #modelNames file
			residualFile='residuals.bin',	#name of residual binary
			derivativeFile='derivs.bin',	#name of derivative binary
			weightFile='weights.bin'

		)
)

## arf experiment class (containing, rootdirectories, condition numbers etc. and preferred sequences)
setClass(
		Class='experiment',
		contains='settings',
		representation=representation(
				path='character', 			#rootdirectory of experiment
				name='character',			#experiment name
				subject.num='numeric',		#number of subjects
				subject.names='character',	#names of subjects
				condition.num='numeric',	#number of conditions
				condition.names='character' #names of condition directories
		
		)
)

## arf (general) settings class (containing general settings on NLM, starting values and range checks)
setClass(
	Class='options',
	representation=representation(
		min.iterlim='numeric',		#NLM settings
		start.method='character',	#which method of determining starting values is used
		start.maxfac='numeric',
		start.vector='numeric',		#vector containing startingvalues (if !start.method=='fixed' only t5 and t6 are used) vector is recycled for regions.
		chk.method='character',		#which method is used to check the range of parameter values
		chk.range='numeric',		#vector containing ranges for each parameter (vector is recycled for regions)		
		sw.type='character'			#method to use with Residuals ('diag','full')
	),
	prototype=prototype(
		min.iterlim=1500,
		start.method='rect',
		start.maxfac=2,
		start.vector=c(0,0,0,0,0,0,.1,.1,.1,100),
		chk.method='imagedim',
		chk.range=c(0,0,0,0,0,0,-.9,-.9,-.9,-1e+64,0,0,0,0,0,0,.9,.9,.9,1e+64),
		sw.type='diag'
	)
)

## make nifti.fileinfo class
setClass(
	Class='nifti.fileinfo',
	representation=representation(
		fullpath='character',		#Full path of nifti file
		filename='character',		#name of nifti file
		filetype='character',		#type (nifti or analyze)
		extension='character',		#extension (.nii, .hdr/.img)
		gzipped='logical',			#gzipped or not
		endian='character'			#endian
	),
	prototype=prototype(
		gzipped=TRUE,
		extension='nii',
		filetype='nifti',
		endian=.Platform$endian
	)
)

## nifti.header class (inherits nifti.fileinfo)
setClass(
	Class='nifti.header',
	contains='nifti.fileinfo',
	representation=representation(
		sizeof_hdr = 'numeric',			#size of header (must be 348))
		data_type =  'character',		
		db_name =  'character',			
		extents = 'numeric',			
		session_error = 'numeric',
		regular = 'character',
		dim_info = 'character',
		dims = 'numeric',				#dimensions (num of dim, dimx, dimy,...)
		intent_p1 = 'numeric',
		intent_p2 = 'numeric',
		intent_p3 = 'numeric',
		intent_code = 'numeric',
		datatype = 'numeric',			#storage data type
		bitpix = 'numeric',				#bits per pixel
		slice_start = 'numeric',		
		pixdim = 'numeric',
		vox_offset = 'numeric',			#offset of data in .nii file
		scl_slope = 'numeric',
		scl_inter = 'numeric',
		slice_end = 'numeric',
		slice_code = 'numeric',
		xyzt_units = 'numeric',
		cal_max = 'numeric',
		cal_min = 'numeric',
		slice_duration = 'numeric',
		toffset = 'numeric',
		glmax = 'numeric',
		glmin = 'numeric',
		descrip = 'character',
		aux_file = 'character',
		qform_code = 'numeric',
		sform_code = 'numeric',
		quatern_b = 'numeric',
		quatern_c = 'numeric',
		quatern_d = 'numeric',
		qoffset_x = 'numeric',
		qoffset_y = 'numeric',
		qoffset_z = 'numeric',
		srow_x = 'numeric',
		srow_y = 'numeric',
		srow_z = 'numeric',
		intent_name = 'character',		#meaning of data
		magic = 'character',			#magicstring
		data.type='character',			#type of data
		data.signed='logical'			#signed data
	),
	prototype=prototype(
		sizeof_hdr = 348,			#size of header (must be 348))
		data_type =  '',		
		db_name =  '',			
		extents = 0,			
		session_error = 0,
		regular = 'r',
		dim_info = '',
		dims = c(3,0,0,0,0,0,0,0),	#dimensions (num of dim, dimx, dimy,...)
		intent_p1 = 0,
		intent_p2 = 0,
		intent_p3 = 0,
		intent_code = 0,
		datatype = 16,			#storage data type
		bitpix = 32,			#bits per pixel
		slice_start = 0,		
		pixdim = c(1,3,3,3,2.8,0,0,0),
		vox_offset = 0,			#offset of data in .nii file
		scl_slope = 1,
		scl_inter = 0,
		slice_end = 0,
		slice_code = 0,
		xyzt_units = 10,
		cal_max = 0,
		cal_min = 0,
		slice_duration = 0,
		toffset = 0,
		glmax = 0,
		glmin = 0,
		descrip = 'ARF3DS4',
		aux_file = '',
		qform_code = 1,
		sform_code = 1,
		quatern_b = 0,
		quatern_c = 0,
		quatern_d = 0,
		qoffset_x = 0,
		qoffset_y = 0,
		qoffset_z = 0,
		srow_x = c(3,0,0,0),
		srow_y = c(0,3,0,0),
		srow_z = c(0,0,3,0),
		intent_name = 'ArfBlobsimdata',	#meaning of data
		magic = 'n+1',					#magicstring
		data.type = 'double',			#type of data
		data.signed = TRUE				#signed data
	)	
)

## fmri data class (inherits nifti.header)
setClass(
	Class='fmri.data',
	contains='nifti.header',
	representation=representation(
		datavec='numeric'
	)
)



## arf data class (containing info on the locations of the data and weightfiles, the dimensions, number of trials, and relevant nifti parameters.)
setClass(
	Class='data',
	representation=representation(
		name='character',			#indicator of data (subject,condition etc.)
		fullpath='character',		#fullpath of files
		betafiles='character',		#vector of char containing trial datafiles
		weightfiles='character',	#vector of char containing weight datafiles
		avgdatfile='character',		#filename of average data
		avgWfile='character',		#filename of average weights
		trials='numeric'			#number of trials
	)
)


## arf model class (containing information on the fitted arf model, it extends the data class)
setClass(
	Class='model',
	contains='data',
	representation=representation(
		modelname='character',		#name of the model (default is region_n)
		modelpath='character',		#path to the model directory
		modeldatapath='character',  #path to the modeldata dir
		residualFile='character',	#Residual Filename
		derivativeFile='character',	#Derivative Filename
		weightFile='character',
		modelDataFile='character',	#niftifilename
		fullmodelDataFile='character',#full niftifilename
		modelFile='character',		#modelFilename
		optionsFile='character',	#optionsFilename
		startFile='character',		#
		convergence='character', 	#convergence information
		iterates='numeric',			#number of iterations
		minimum='numeric',			#minimum of objective function
		estimates='numeric',		#vector of parameter estimates (t1r1..t6r1,t1r2..t6r2,t1rR..t6rR)
		hessian='matrix',			#hessian matrix
		sandwichmethod='character',	#sandwichmethod
		varcov='matrix',			#variance covariance matrix (full form)
		warnings='character',		#warnings (pos def var/covar etc.)
		fit='numeric',				#fit value (BIC)
		wald='ANY',					#object of class 'wald'
		regions='numeric',			#number of fitted regions
		startval='numeric',			#vector of starting values
		proctime='matrix',			#processing time
		valid='logical'				#is model valid (converged and no warnings)
	),
	prototype=prototype(
			valid=FALSE,
			proctime=matrix(0,1,2)
	)
)



## arf sequence class (containing info (fit, valid) on a sequence of models)
setClass(
	Class='sequence',
	representation=representation(
		current='numeric',			#current number of regions in model
		regions='numeric',			#vector of regions to fit (can be sequential or any other combination)
		mnames='character',			#vector of names of models
		fit='numeric',				#vector of fit measures (to evaluate best fit)
		minimum='numeric',			#which region has the minimum
		valid='numeric'				#vector of validity of solutions (all estimates and variances ok)
	),
	prototype=prototype(
			current=1,
			minimum=0
	)
)


## wald statistics class
setClass(
	Class='wald',
	representation=representation(
		design='matrix',	#design
		stats='matrix',		#statistic
		df1='numeric',		#df1
		df2='numeric',		#df2
		pvalues='matrix'	#pvalues
	)
	
)


## arf analysis class (not yet implemented)
setClass(
		Class='analysis',
		representation=representation(
				name='character',			#name of the analysis
				modelobjects='ANY',			#names of model objects in the analysis
				designmatrix='numeric',		#designmatrix of analysis
				rfx='logical'				#perform random-effects analysis?
		)
)


## arf sims class (only for simulation of data) (not yet implemented)
setClass(
		Class='sims',
		representation=representation(
				numsims='numeric',			#number of simulations
				numtrials='numeric',		#number of trials
				theta='numeric',			#vector of parameter values to simulate
				shapemodel='character',		#which shape model of signal
				noisemodel='character',		#which noise calc method
				noiseFWHM='numeric',		#FWHM of noise
				imageFWHM='numeric',		#FWHM of signal+noise smoother		
				sequence='numeric',			#sequence of regions to be fit
				SNR='numeric'				#which signal to noise ratio is used?
		)
)


