# ARF S4 Class Definition file
# Wouter D. Weeda, University of Amsterdam
###############################################################################

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
			update=1
		)
)

## arf (general) settings class (containing general settings on NLM, starting values and range checks)
setClass(
	Class='settings',
	representation=representation(
		min.iterlim='numeric',		#NLM settings
		start.method='character',	#which method of determining starting values is used
		start.maxfac='numeric',
		start.vector='numeric',		#vector containing startingvalues (if !start.method=='fixed' only t5 and t6 are used) vector is recycled for regions.
		chk.method='character',		#which method is used to check the range of parameter values
		chk.range='numeric',		#vector containing ranges for each parameter (vector is recycled for regions)		
		warn='numeric'				#suppress warnings
	),
	prototype=prototype(
		min.iterlim=1500,
		start.method='rect',
		start.maxfac=2,
		start.vector=c(0,0,0,0,.1,100),
		chk.method='imagedim',
		chk.range=c(0,0,0,0,-.9,-1e+64,0,0,0,0,.9,1e+64),
		warn=-1
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
		gzipped=FALSE,
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
		dims = c(2,0,0,0,0,0,0,0),	#dimensions (num of dim, dimx, dimy,...)
		intent_p1 = 0,
		intent_p2 = 0,
		intent_p3 = 0,
		intent_code = 0,
		datatype = 16,			#storage data type
		bitpix = 32,			#bits per pixel
		slice_start = 0,		
		pixdim = c(0,0,0,0,0,0,0,0),
		vox_offset = 0,			#offset of data in .nii file
		scl_slope = 0,
		scl_inter = 0,
		slice_end = 0,
		slice_code = 0,
		xyzt_units = 0,
		cal_max = 0,
		cal_min = 0,
		slice_duration = 0,
		toffset = 0,
		glmax = 0,
		glmin = 0,
		descrip = 'ARFv1b1',
		aux_file = '',
		qform_code = 0,
		sform_code = 0,
		quatern_b = 0,
		quatern_c = 0,
		quatern_d = 0,
		qoffset_x = 0,
		qoffset_y = 0,
		qoffset_z = 0,
		srow_x = c(0,0,0,0),
		srow_y = c(0,0,0,0),
		srow_z = c(0,0,0,0),
		intent_name = 'character',		#meaning of data
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
		datafiles='character',		#vector of char containing trial datafiles
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
		convergence='character', 	#convergence information
		minimum='numeric',			#minimum of objective function
		estimates='numeric',		#vector of parameter estimates (t1r1..t6r1,t1r2..t6r2,t1rR..t6rR)
		hessian='matrix',			#hessian matrix
		varcov='matrix',			#variance covariance matrix (full form)
		warnings='character',		#warnings (pos def var/covar etc.)
		fit='numeric',				#fit value (BIC)
		wald='ANY',					#object of class 'wald'
		regions='numeric',			#number of fitted regions
		startval='numeric',			#vector of starting values
		proctime='numeric',			
		valid='logical'				#is model valid (converged and no warnings)
	),
	prototype=prototype(
			valid=FALSE
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


