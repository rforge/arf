#############################################
# arf3DS4 S4 SIMTOOLS FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

simBlobs3D <- function(betadir,weightdir,templatedata,model=c('gauss','square','snr0'),regions=1,theta=c(32,32,16,5,5,3,.1,.3,.2,1000),sq_par=c(12,12,8,4,4,3,1000),dims=c(64,64,32),snr=10,trials=10,tslen=101,noisesmooth=0,maxb=c('max','box'),box=2) {
		
	#set SNR
	tsnr = snr / sqrt(trials)
	
	#set dimensions of data
	.nifti.header.dims(templatedata)[5:7] <- 1
	.nifti.header.dims(templatedata)[1] <- 3
	
	.nifti.header.dims(templatedata)[2] <- dims[1]
	.nifti.header.dims(templatedata)[3] <- dims[2]
	.nifti.header.dims(templatedata)[4] <- dims[3]
		
	dimx <- .nifti.header.dims(templatedata)[2]
	dimy <- .nifti.header.dims(templatedata)[3]
	dimz <- .nifti.header.dims(templatedata)[4]
	
	n <- dimx*dimy*dimz
	
	for(trial in 1:trials) {
	
		model <- match.arg(model[1],c('gauss','square','snr0'))
		
		#make the model
		if(model=='gauss') newdata <- .C('gauss',as.double(theta),as.integer(regions*10),as.integer(dimx),as.integer(dimy),as.integer(dimz),as.double(numeric(n)))[[6]]
		if(model=='square') {
			newdata <- array(0,dim=c(dimx,dimy,dimz))
			newdata[(sq_par[1]-sq_par[4]):(sq_par[1]+sq_par[4]),(sq_par[2]-sq_par[5]):(sq_par[2]+sq_par[5]),(sq_par[3]-sq_par[6]):(sq_par[3]+sq_par[6])]=sq_par[6]
			newdata = as.vector(newdata)
		}
		if(model=='snr0') {
			newdata <- rep(0,n)
		}
		
		
		#max beta
		maxb <- match.arg(maxb[1],c('max','box'))
		if(maxb=='max') maxbeta <- max(newdata)
		
		if(maxb=='box') {
			y=newdata
			dim(y) = c(dimx,dimy,dimz)
			maxbeta <- mean(y[(theta[1]-box):(theta[1]+box),(theta[2]-box):(theta[2]+box),(theta[3]-box):(theta[3]+box)])
		}
		
		if(model=='snr0') {
			maxbeta <- 1
			tsnr=1
		}
		
		#call .C routine for signal and weights
		sigweight = .C('simTS',as.double(newdata),as.double(maxbeta),as.double(tsnr),as.integer(tslen),as.integer(n),as.double(numeric(n)),as.double(numeric(n)))
		
		signal = sigweight[[6]]
		weights = sigweight[[7]]
			
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
		.nifti.header.filename(templatedata) <- paste('weight',trial,sep='')
		
		#open files based on gzippedness 
		if(.nifti.header.gzipped(templatedata)==TRUE) {
			fn <- paste(pathname,.Platform$file.sep,'weight',trial,'.',extension,'.gz',sep='')
			#file.create(fn)
		} else {
			fn <-paste(pathname,.Platform$file.sep,'weight',trial,'.',extension,sep='')
			#file.create(fn)
		}		
		
		writeData(templatedata,weights)
		
	}
	
	return(invisible(TRUE))
	
}

checkSNR <- function(snr,trials=10,tslen=101,smooth=F) {
	
	makeExpDirs('~/Desktop','snrcheck','testsubject','testcondition')
	exp = suppressWarnings(loadExp('~/Desktop/snrcheck'))
	fn = paste('~/Desktop/snrcheck/subjects/testsubject/conditions/testcondition/data/')
	
	if(snr!=0) {
		simBlobs3D(paste(fn,'beta',sep=''),paste(fn,'weights',sep=''),new('fmri.data'),model='square',sq_par=c(12,12,8,4,4,3,100),dims=c(64,64,32),snr=snr,noisesmooth=smooth,trials=trials,tslen=tslen,maxb=c('max')) 
		
	} else {
		simBlobs3D(paste(fn,'beta',sep=''),paste(fn,'weights',sep=''),new('fmri.data'),model='snr0',sq_par=c(12,12,8,4,4,3,100),dims=c(64,64,32),snr=snr,noisesmooth=smooth,trials=trials,tslen=tslen,maxb=c('max'))
	}
	
	
	setExp('~/Desktop/snrcheck',over=T)
	exp = loadExp('~/Desktop/snrcheck')
	createAllAverages(exp)
	
	for(i in 1:trials) {
		
		dat = readData(paste(fn,'beta/data',i,'.nii.gz',sep=''))
		W =  readData(paste(fn,'weights/weight',i,'.nii.gz',sep=''))
		
		dims = c(dat@dims[2],dat@dims[3],dat@dims[4])
		
		dat = dat@datavec
		W = W@datavec
		tvec = dat/sqrt(W)
		
		dim(tvec) = dims
		
		b=4
		
		mean_signal = mean(tvec[(12-b):(12+b),(12-b):(12+b),(8-b):(8+b)])
		sd_noise = sd(tvec[(56-b):(56+b),(56-b):(56+b),(28-b):(28+b)])
	
		st_snr = mean_signal/sd_noise
		cat('Trial',i,'SNR=',st_snr,' (',snr/sqrt(trials),')\n')
	}

	bet = readData(paste(fn,'avg/avgdata.nii.gz',sep=''))
	W = readData(paste(fn,'avg/avgweight.nii.gz',sep=''))
	new_dat = bet@datavec / sqrt(W@datavec)
	dim(new_dat) = dims
	
	tval_dat = readData(paste(fn,'avg/avgtstat.nii.gz',sep=''))
	tval_dat = tval_dat@datavec
	dim(tval_dat) = dims
	
		
	quartz(width=4,height=8)
	layout(matrix(1:2,,1))
	hist(tval_dat)
	hist(new_dat)
	
	
	mean_signal = mean(tval_dat[(12-b):(12+b),(12-b):(12+b),(8-b):(8+b)])
	sd_noise = sd(tval_dat[(56-b):(56+b),(56-b):(56+b),(28-b):(28+b)])
	overall_snr = mean_signal/sd_noise
	
	cat('Overall SNR=',overall_snr,' (',snr,')\n')
	
}


blobMe <- function(dir,name,sims=10,model='gauss',regions=1,theta=c(32,32,16,5,5,3,.1,.3,.2,1000),snr=10,trials=10,dims=c(64,64,32)) 
{

	sp <- .Platform$file.sep
	
	cat('creating experiment',name,'with',trials,'trials @snr',snr,'\n')
	
	settings <- new('settings')
	experiment <- makeExpDirs(path=dir,name=name,subjectind='lerxst',conditionind=1:sims,settings=settings)

	path <- .experiment.path(experiment)
	
	tempdata <- new('fmri.data')
	
	subd <- paste(path,sp,.settings.subjectDir(settings),sep='')
	
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		sn <- paste(subd,sp,.settings.subjectPrefix(settings),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.settings.conditionDir(settings),sep='')
				
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			cat('processing condition',cdirs,'[',trials,'] ... ')
			
			#create individual condition dirs
			cn <- paste(subc,sp,.settings.conditionPrefix(settings),.experiment.condition.names(experiment)[cdirs],sep='')
			dn <- paste(cn,sp,.settings.dataDir(settings),sep='')
			
			bdir <- paste(dn,sp,.settings.betaDir(settings),sep='')
			wdir <- paste(dn,sp,.settings.weightsDir(settings),sep='')
			
 			simBlobs3D(bdir,wdir,tempdata,model=model,regions=regions,theta=theta,sq_par=theta,dims=dims,snr=snr,trials=trials,tslen=101,noisesmooth=0,maxb='max',box=2) 
				
			cat('ok\n')
		}
	}
	exp = setExp(path)
	cat('creating averages...')
	createAllAverages(exp)
	cat('ok\n')
}


simBlobsST <- function(betadir,weightdir,templatedata,model='gauss',regions=3,theta=c(12,12,24,3,4,3,.1,.1,.3,100,32,32,6,2,2,5,.1,.2,.2,30,12,56,12,5,4,2,.4,.1,.2,100),snr=5,noisesmooth=0,trials=50,tslen=101) {
	
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
	Sigma=matrix(c(33^2,24^2,15^2,24^2,33^2,5^2,15^2,5^2,33^2),3)
	
	print(Sigma)
	
	thetamean=mvrnorm(n=50,mu=c(100,30,100),Sigma=Sigma)
	
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




