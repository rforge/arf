#############################################
# arf3DS4 S4 SIMTOOLS FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

simBlobs3D <- function(betadir,weightdir,templatedata,model='gauss',regions=1,theta=c(9,9,9,3,2,2,.1,.3,.2,100),snr=10,noisesmooth=0,trials=10,tslen=101) {
	
	.nifti.header.dims(templatedata)[5:7] <- 1
	.nifti.header.dims(templatedata)[1] <- 3
	
	.nifti.header.dims(templatedata)[2] <- 18
	.nifti.header.dims(templatedata)[3] <- 18
	.nifti.header.dims(templatedata)[4] <- 18
		
	dimx <- .nifti.header.dims(templatedata)[2]
	dimy <- .nifti.header.dims(templatedata)[3]
	dimz <- .nifti.header.dims(templatedata)[4]
	
	n <- dimx*dimy*dimz
	
	for(trial in 1:trials) {
		
		#make the model
		newdata <- .C('gauss',as.double(theta),as.integer(regions*10),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(numeric(n)))[[6]]
		maxbeta <- max(newdata)
		
		#fill matrices (recylce model for each timepoint)
		datamat <- matrix(newdata,n,tslen,byrow=F)
		errormat <- matrix(rnorm(tslen*n,0,maxbeta/snr)*sqrt(tslen),n,tslen)
		
		#make signal
		signalmat=datamat+errormat
		
		#create vectors
		signal <- apply(signalmat,1,mean)
		weights <- apply(signalmat,1,sd)
		
		pathname <- betadir
		.nifti.header.fullpath(templatedata) <- pathname
		.nifti.header.filename(templatedata) <- paste('data',trial,sep='')
		
		if(.nifti.header.extension(templatedata)=='hdr') extension='img' else extension <- .nifti.header.extension(templatedata)
		
		#open files based on gzippedness 
		if(.nifti.header.gzipped(templatedata)==TRUE) {
			fn <- paste(pathname,.Platform$file.sep,'data',trial,'.',extension,'.gz',sep='')
			#file.create(fn)
		} else {
			fn <-paste(pathname,.Platform$file.sep,'data',trial,'.',extension,sep='')
			#file.create(fn)
		}		
		
		writeData(templatedata,signal)
		
		pathname <- weightdir		
		.nifti.header.fullpath(templatedata) <- pathname
		.nifti.header.filename(templatedata) <- paste('data',trial,sep='')
		
		#open files based on gzippedness 
		if(.nifti.header.gzipped(templatedata)==TRUE) {
			fn <- paste(pathname,.Platform$file.sep,'data',trial,'.',extension,'.gz',sep='')
			#file.create(fn)
		} else {
			fn <-paste(pathname,.Platform$file.sep,'data',trial,'.',extension,sep='')
			#file.create(fn)
		}		
		
		writeData(templatedata,weights)
		cat('writing',fn,'\n')
	}
	
	return(invisible(TRUE))
	
}

blobMe <- function(dir,name,sims=10,regions=1,theta=c(9,9,9,2,3,2,.1,.2,.2,100),snr=10,trials=10,connectivity=F) 
{

	sp <- .Platform$file.sep
	
	settings <- new('settings')
	
	experiment <- makeExpDirs(path=dir,name=name,subjectind='lerxst',conditionind=1:sims,settings=settings)

	path <- .experiment.path(experiment)
	
	tempdata <- new('nifti.header')
	
	subd <- paste(path,sp,.settings.subjectDir(settings),sep='')
	
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		sn <- paste(subd,sp,.settings.subjectPrefix(settings),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.settings.conditionDir(settings),sep='')
				
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			#create individual condition dirs
			cn <- paste(subc,sp,.settings.conditionPrefix(settings),.experiment.condition.names(experiment)[cdirs],sep='')
			dn <- paste(cn,sp,.settings.dataDir(settings),sep='')
			
			bdir <- paste(dn,sp,.settings.betaDir(settings),sep='')
			wdir <- paste(dn,sp,.settings.weightsDir(settings),sep='')
			
			if(connectivity)  simBlobsST(bdir,wdir,tempdata,model='gauss',regions=3,snr=snr,noisesmooth=0,trials=50,tslen=101)
				else simBlobs3D(bdir,wdir,tempdata,model='gauss',regions=regions,theta=theta,snr=snr,noisesmooth=0,trials=trials,tslen=101)
			
		}
	}

	loadExp(paste(path,sp,.experiment.expRda(experiment),sep=''))	
}

arfSim <- function(exp,condpath,cnvec,regions=1,swmode='diag',waldcalc=F) {
			
	for(i in cnvec) {
		cat('Processing condition-sim',i,'...')
		fn <- paste(condpath,i,'/data/data.Rda',sep='')
		arfdata <- loadRda(fn)
		
		arfdata <- createAverages(exp,arfdata)
		arfmodel <- newModel(arfdata,exp,paste('sim_',regions,'_region',sep=''))
		.model.regions(arfmodel) <- regions
		
		options=loadOptions(arfmodel)
		
		.options.start.maxfac(options)=3
		
		arfmodel <- determineStartRect(arfmodel,options=options)
		arfmodel <- fitModel(arfmodel)
		arfmodel <- BIC(arfmodel)
		makeDerivs(arfmodel)
		makeResiduals(arfmodel)
		
		.model.sandwichmethod(arfmodel) <- swmode
		
		arfmodel <- varcov(arfmodel)
		if(waldcalc) arfmodel <- wald(arfmodel)
		
		saveModel(arfmodel)
		cat('ok\n')
	}

}

simBlobsST <- function(betadir,weightdir,templatedata,model='gauss',regions=3,theta=c(12,12,24,3,4,3,.1,.1,.3,100,32,32,6,2,2,5,.1,.2,.2,100,12,56,12,5,4,2,.4,.1,.2,100),snr=5,noisesmooth=0,trials=50,tslen=101) {
	
	require(MASS)
	
	.nifti.header.dims(templatedata)[5:7] <- 1
	.nifti.header.dims(templatedata)[1] <- 3
	
	.nifti.header.dims(templatedata)[2] <- 64
	.nifti.header.dims(templatedata)[3] <- 64
	.nifti.header.dims(templatedata)[4] <- 32
	
	dimx <- .nifti.header.dims(templatedata)[2]
	dimy <- .nifti.header.dims(templatedata)[3]
	dimz <- .nifti.header.dims(templatedata)[4]
	
	n <- dimx*dimy*dimz
	

	#for connectivity
	Sigma=matrix(c(33^2,24^2,0,24^2,33^2,0,0,0,33^2),3)
	
	thetamean=mvrnorm(n=50,mu=c(100,100,100),Sigma=Sigma)
	
	realamp=cbind(thetamean)
	
	print(cov(thetamean))
	print(cor(thetamean))
	
	cat('Average SNR:',snr,'\n')
	snr=snr/sqrt(trials)
	cat('Single trial SNR:',snr,'\n')
	
	
	for(trial in 1:trials) {
		
		#for connectivity
		theta[10]=thetamean[trial,1]
		theta[20]=thetamean[trial,2]
		theta[30]=thetamean[trial,3]
		
		#make the model
		newdata <- .C('gauss',as.double(theta),as.integer(regions*10),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(numeric(n)))[[6]]
		maxbeta <- max(newdata)
		
		#fill matrices (recylce model for each timepoint)
		datamat <- matrix(newdata,n,tslen,byrow=F)
		errormat <- matrix(rnorm(tslen*n,0,maxbeta/snr)*sqrt(tslen),n,tslen)
		
		#make signal
		signalmat=datamat+errormat
		
		#create vectors
		signal <- apply(signalmat,1,mean)
		weights <- apply(signalmat,1,sd)
		
		pathname <- betadir
		.nifti.header.fullpath(templatedata) <- pathname
		.nifti.header.filename(templatedata) <- paste('data',trial,sep='')
		
		if(.nifti.header.extension(templatedata)=='hdr') extension='img' else extension <- .nifti.header.extension(templatedata)
		
		#open files based on gzippedness 
		if(.nifti.header.gzipped(templatedata)==TRUE) {
			fn <- paste(pathname,.Platform$file.sep,'data',trial,'.',extension,'.gz',sep='')
			#file.create(fn)
		} else {
			fn <-paste(pathname,.Platform$file.sep,'data',trial,'.',extension,sep='')
			#file.create(fn)
		}		
		
		writeData(templatedata,signal)
		
		pathname <- weightdir		
		.nifti.header.fullpath(templatedata) <- pathname
		.nifti.header.filename(templatedata) <- paste('data',trial,sep='')
		
		#open files based on gzippedness 
		if(.nifti.header.gzipped(templatedata)==TRUE) {
			fn <- paste(pathname,.Platform$file.sep,'data',trial,'.',extension,'.gz',sep='')
			#file.create(fn)
		} else {
			fn <-paste(pathname,.Platform$file.sep,'data',trial,'.',extension,sep='')
			#file.create(fn)
		}		
		
		writeData(templatedata,weights)
		cat('writing',fn,'\n')
		
		
		
	}
	cat('\n\n\n')
	#return(list(means=thetamean,cor=cor(thetamean),cov=cov(thetamean)))
	return(signal)
}

