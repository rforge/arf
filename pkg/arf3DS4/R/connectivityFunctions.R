#############################################
# arf3DS4 S4 CONNECTIVITY FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#fitConnectivity


fitConnectivity <- 
function(arfmodel,regs=1:.model.regions(arfmodel))
#make a connectivity estimate for a model solution
{
	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	n = .nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4]
	
	#make model design matrix
	X = matrix(NA,n,length(regs))
	theta = matrix(.model.estimates(arfmodel),.model.params(arfmodel))
	
	#cat('create model design matrix\n')
	p=1
	for(i in regs) {
		thetavec = as.vector(theta[,i])
		X[,p] = .C('gauss',as.double(thetavec),as.integer(.model.params(arfmodel)),as.integer(.nifti.header.dims(headinf)[2]),as.integer(.nifti.header.dims(headinf)[3]),as.integer(.nifti.header.dims(headinf)[4]),as.double(numeric(.nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4])))[[6]]
		p=p+1
	}
	
	B = numeric(0)
	
	#open functional file
	filelist <- .model.betafiles(arfmodel)
	path <- .model.funcDir(arfmodel)
	sp <- .Platform$file.sep
	
	for(filename in filelist) {
		if(file.exists(filename)) {
		
			info <- getFileInfo(filename)
			dirname <- .nifti.fileinfo.filename(info)
	
			func <- loadRda(paste(path,sp,dirname,sp,.data.funcRda(arfmodel),sep=''))
			timings <- .functional.timings(func)
	
			funcdata <- readData(paste(.functional.fullpath(func),sp,.functional.functionaldata(func),sep=''))
			funcvolume <- .fmri.data.datavec(funcdata)
			
			dim(funcvolume) = c(.fmri.data.dims(funcdata)[2],.fmri.data.dims(funcdata)[3],.fmri.data.dims(funcdata)[4],.fmri.data.dims(funcdata)[5])
			trialdata <- readData(filename)

			#cat('regress volumes\n')
			
			#regress the model to the volumes 
			b = matrix(NA,length(regs),length(timings))
			#cat(timings,'\n')
			
			p=1
			for(tm in timings) {
				y = as.vector(funcvolume[,,,tm+1])
				if(length(y)==nrow(X)) b[,p] = solve(t(X)%*%X)%*%t(X)%*%y
				p=p+1
			}
			
			B = cbind(B,b)
			
		} else warning('No betafile found to match functional data to')
	}
	
	return(B)
	
}
