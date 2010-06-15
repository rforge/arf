#############################################
# arf3DS4 S4 CONNECTIVITY FUNCTIONS			#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#fitConnectivity
#gamma.fmri
#convol.fmri
#makeSingleTrialEvents
#tscor
#partialCor
#cor.test.matrix
#processCorrelations
#roiConnectivity

fitConnectivity <- 
function(arfmodel,funcfilename='single_events.nii.gz')
#make a connectivity estimate for a model solution
{
	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	n = .nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4]
	regs = 1:.model.regions(arfmodel)
	
	#make model design matrix
	X = matrix(NA,n,length(regs))
	theta = matrix(.model.estimates(arfmodel),.model.params(arfmodel))
	
	#set theta amplitudes to one (all)
	theta[10,]=rep(1,.model.regions(arfmodel))
		
	p=1
	for(i in regs) {
		thetavec = as.vector(theta[,i])
		X[,p] = .C('gauss',as.double(thetavec),as.integer(.model.params(arfmodel)),as.integer(.nifti.header.dims(headinf)[2]),as.integer(.nifti.header.dims(headinf)[3]),as.integer(.nifti.header.dims(headinf)[4]),as.double(numeric(.nifti.header.dims(headinf)[2]*.nifti.header.dims(headinf)[3]*.nifti.header.dims(headinf)[4])))[[6]]
		p=p+1
	}
	
	
	funcdata = readData(paste(.model.funcDir(arfmodel),.Platform$file.sep,funcfilename,sep=''))	
	funcvolume = .fmri.data.datavec(funcdata)
	dim(funcvolume) = c(.fmri.data.dims(funcdata)[2],.fmri.data.dims(funcdata)[3],.fmri.data.dims(funcdata)[4],.fmri.data.dims(funcdata)[5])
	
	b =  matrix(NA,length(regs),.fmri.data.dims(funcdata)[5])
	Xp = solve(t(X)%*%X)%*%t(X)
	
	p=1
	for(tm in 1:.fmri.data.dims(funcdata)[5]) {
		y = as.vector(funcvolume[,,,tm])
		b[,p] = Xp%*%y
		p=p+1
	}
	
	
	#make correlation matrix and return matrices
	cmat <- cor.test.matrix(t(b),alpha=.05,bonf=F) 
	out <- list(ts=b,cor=cmat$cor,p=cmat$p)
	attr(out,'class') <- 'arfcorrelation'
		
	return(out)
	
}

makeSingleTrialEvents <- 
function(arfmodel,sefilename='single_events')
#make single trial estimates
{
	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	
	#open functional file
	filelist <- .model.betafiles(arfmodel)
	path <- .model.funcDir(arfmodel)
	sp <- .Platform$file.sep
	
	datavec = numeric(0)
	totdim = 0
	
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			info <- getFileInfo(filename)
			dirname <- .nifti.fileinfo.filename(info)
			
			func <- loadRda(paste(path,sp,dirname,sp,.data.funcRda(arfmodel),sep=''))
			timings <- .functional.timings(func)
		
			#make array of fmri volume
			fmrivolume = readData(paste(.functional.fullpath(func),sp,.functional.functionaldata(func),sep=''))
			funcvolume = .fmri.data.datavec(fmrivolume)
			dim(funcvolume) = c(.fmri.data.dims(fmrivolume)[2],.fmri.data.dims(fmrivolume)[3],.fmri.data.dims(fmrivolume)[4],.fmri.data.dims(fmrivolume)[5])
			n = .fmri.data.dims(fmrivolume)[2]*.fmri.data.dims(fmrivolume)[3]*.fmri.data.dims(fmrivolume)[4]
		
			#create timeseries in seconds and create double gamma
			tslen = round(.fmri.data.dims(fmrivolume)[5] * .fmri.data.pixdim(fmrivolume)[5])
			stick = rep(0,tslen)
			hrf <- gamma.fmri(1:tslen) 
			stick[round(timings)]=1
			vecnums = which(stick==1)
	
			#define design matrices and outputs
			X = matrix(NA,.fmri.data.dims(fmrivolume)[5],length(vecnums))
			beta = matrix(0,n,length(vecnums))
			
			#create single-trial columns in design matrix
			for(i in 1:length(vecnums)) {
				st_protocol = rep(0,tslen)
				st_protocol[vecnums[i]]=1
				bold = convol.fmri(hrf,st_protocol)
				bold = bold[seq(1,tslen,.fmri.data.pixdim(fmrivolume)[5])]
				X[,i] = bold 
	
			}
			
			#Solve the LS equation for each voxel (on DEMEANED data)
			Xp = solve(t(X)%*%X)%*%t(X)
				
			i=1;
			for(z in 1:.fmri.data.dims(fmrivolume)[4]) {
				for(y in 1:.fmri.data.dims(fmrivolume)[3]) {
					for(x in 1:.fmri.data.dims(fmrivolume)[2]) {
						if(.model.mask(arfmodel)[i]==1) {
							dat = as.vector(funcvolume[x,y,z,])
							dat = dat-mean(dat)
							beta[i,] = as.vector(Xp%*%dat)
						}
						i=i+1;
					}
				}
			}
			
			#concatenate datavecs and increase dimensions of volume
			datavec = c(datavec,as.vector(beta))
			totdim = totdim + length(vecnums)

		}
	}
	
	#make new file for filetered single events
	.fmri.data.filename(fmrivolume) = sefilename
	.fmri.data.dims(fmrivolume)[5] = totdim
	.fmri.data.pixdim(fmrivolume)[5] = 1
	.fmri.data.datavec(fmrivolume) = datavec
	.fmri.data.fullpath(fmrivolume) = .model.funcDir(arfmodel)
	writeData(fmrivolume,.fmri.data.datavec(fmrivolume))
	
	return(fmrivolume)
	
	
}

gamma.fmri <-
#double gamma function by Lourens Waldorp
function(t,a1=6,a2=12,b1=0.9,b2=0.9,ce=0.35,...)
{
	hrf <- ((t/a1*b1)^a1)*exp(-1*(t - a1*b1)/b1) - 
			ce*((t/a2*b2)^a2)*exp(-1*(t - a2*b2)/b2);
	
	kernSize <- length(hrf[abs(hrf)>1e-4]);
	attr(hrf,"kernSize") <- kernSize;
	
	return(hrf)
}

convol.fmri <-
#convolve fmri timeseries by Lourens Waldorp
function(hrf, prot, ...) 
{
	if(is.null(attr(hrf,"kernSize"))){
		tmp  <- hrf;
		hrf  <- prot;
		prot <- tmp;
	}
	window <- 1:length(prot);
	win <- length(window);
	protExt <- c(rep(0,win),prot);    
	conv <- numeric(0);
	for(i in window){
		conv[i] <- sum( protExt[i:(win+i-1)] * hrf[win:1] );
	}
	
	convTime <- as.double(conv[window]);
	convSample <- rep(NA,length(window));

	stimON <- rep(NA,win);
	stimON[which(prot[window]!=0)] <- 0;
	
	attr(conv,"time") <- convTime;
	attr(conv,"stimON") <- stimON[1:length(window)];
	attr(conv,"stim") <- prot;
	attr(conv,"hrf")  <- hrf;
	class(conv) <- "fmri"
	
	return(conv)
}

cor.test.matrix <- 
function(data,alpha=.05,bonf=T) 
#calculate correlation test of a matrix
{
	
	pmat = matrix(0,ncol(data),ncol(data))
	cormat = matrix(1,ncol(data),ncol(data))
	sigmat = matrix(0,ncol(data),ncol(data))
	numcors = (ncol(data)*ncol(data)-ncol(data))/2
	siglist = matrix(NA,numcors,4)
	colnames(siglist) = c('row','col','est','p')
	
	if(bonf) p = alpha/numcors else p=alpha
	#cat('p-value:',p,'\n')
	
	i=1;
	for(row in 1:(ncol(data)-1)) {
		for(col in (row+1):ncol(data)) {	
			if((row+1)<=ncol(data)) {
				ct = cor.test(data[,row],data[,col])
				cormat[row,col] = cormat[col,row] = ct$estimate
				pmat[row,col] = pmat[col,row] = ct$p.value
				if(ct$p.value<p) {
					sigmat[row,col]=1
					siglist[i,] = c(row,col,ct$estimate,ct$p.value)
				}
				i = i + 1
			}	
		}
	}
	
	pcor = partialCor(cormat,ncol(data))
	
	delsig = which(is.na(siglist[,1]))
	if(length(delsig)>0) siglist = siglist[-delsig,]
	#o = order(abs(siglist[,3]),decreasing=T)
	#siglist = siglist[o,]
	
	corlist= list(cor=cormat,p=pmat,sig=sigmat,pcor=pcor,siglist=siglist)
	
	return(corlist)
}


tsCor <-
function(tsmat,regnames=NULL) 
#calculate correlation of timeseries matrix
{
	
	R = cor(t(tsmat))
	if(!is.null(regnames)) {
		rownames(R) = regnames
		colnames(R) = regnames
	}

	return(R)
}

partialCor <-
function(R,n)
#calculate partial correlations
{
	Ri=solve(R)
	pC=sigPc=matrix(NA,nrow(R),ncol(R))
	for(col in 1:ncol(R)) {
		for(row in 1:nrow(R)) {
			pC[row,col] = (-1*Ri[row,col]) / sqrt((Ri[row,row]) * (Ri[col,col]))
			tval = pC[row,col]/sqrt((1-pC[row,col]^2)/(n-2))
			sigPc[row,col]=dt(tval,(n-2))
			
		}
	}
	
	return(list(pcor=pC,p=sigPc))
	
}


differenceCor <-
function(c1,c2,n1,n2=n1) 
#test if correlations are different
{
	
	diffscore = zval = pval = z1 = z2 = matrix(0,dim(c1)[1],dim(c1)[2])
	
	for(i in 1:dim(c1)[1]) {
		for(j in 1:dim(c1)[2]) {
			
			#fischer z transform correlations
			zscore1 = log(abs((c1[i,j]+1)/(c1[i,j]-1)))/2
			zscore2 = log(abs((c2[i,j]+1)/(c2[i,j]-1)))/2
			
			#reverse the sign if correlations are negative
			if(c1[i,j]<0 & zscore1>0) zscore1=zscore1*-1
			if(c2[i,j]<0 & zscore2>0) zscore2=zscore2*-1
			
			#calculate the difference between z values
			diffz = zscore1-zscore2
			
			#calculate standard error of correlations
			SE = sqrt((1/(n1 - 3)) + (1/(n2 - 3)))
			
			#calculate z-value of the difference
			zv = diffz/SE		
			
			#check if zv=NaN (only when one of the correlations is one)
			if(is.nan(zv)) {
				z1[i,j]=0
				z2[i,j]=0
				zval[i,j] = 0
				diffscore[i,j] = 0
				pval[i,j]=1
			} else {
				#fill in matrices with zscores/differences/zvaluesdifference and p-values
				z1[i,j]=zscore1
				z2[i,j]=zscore2
				zval[i,j]=zv
				diffscore[i,j]=diffz
				pval[i,j] = dnorm(zval[i,j])
			} 
		}
	}
	return(list(z1=z1,z2=z2,dif=diffscore,z=zval,pval=pval))
}


roiConnectivity <-
function(arfmodel,roidata=setIsoContour(arfmodel,95),funcfilename='single_events.nii.gz',type=c('avg','ev'),evmodel=c('spatial','spatiotemporal'))
{
	#match type argument
	type = match.arg(type,c('avg','ev'))
	evmodel = match.arg(evmodel,c('spatial','spatiotemporal'))
	
	#make isocontours of model estimates
	roidataarray = fmri2array(roidata)
	roi = vector('list',.fmri.data.dims(roidata)[5])
	for(i in 1:.fmri.data.dims(roidata)[5]) roi[[i]] = which(as.vector(roidataarray[,,,i])>0)
		
	#load functional data
	funcdata = readData(paste(.model.funcDir(arfmodel),.Platform$file.sep,funcfilename,sep=''))	
	funcvolume = .fmri.data.datavec(funcdata)
	dim(funcvolume) = c(.fmri.data.dims(funcdata)[2],.fmri.data.dims(funcdata)[3],.fmri.data.dims(funcdata)[4],.fmri.data.dims(funcdata)[5])
	
	#create time-series for each volume and blob
	b =  matrix(NA,.fmri.data.dims(roidata)[5],.fmri.data.dims(funcdata)[5])

	#create weigthed average first spatial eigenvector
	if(type=='ev') {
		
		if(evmodel=='spatiotemporal') {
			
			eigenvec = vector('list',.fmri.data.dims(roidata)[5])
			
			for(blob in 1:.fmri.data.dims(roidata)[5]) {
				timebyvox = matrix(NA,.fmri.data.dims(funcdata)[5],length(roi[[blob]]))
				
				for(tm in 1:.fmri.data.dims(funcdata)[5]) {
					timebyvox[tm,] = as.vector(funcvolume[,,,tm])[roi[[blob]]]
				}
				
				blobsvd = svd(timebyvox)
				eigenvec[[blob]] = (blobsvd$u[,1]*blobsvd$d[1])%*%t(blobsvd$v[,1])
			
			}
		}
		
		if(evmodel=='spatial') {
			
			eigenvec = vector('list',.fmri.data.dims(roidata)[5])
			avgdata = readData(.model.avgdatfile(arfmodel))
			
			for(blob in 1:.fmri.data.dims(roidata)[5]) {
				timebyvox = matrix(NA,.fmri.data.dims(funcdata)[5],length(roi[[blob]]))
				
				for(tm in 1:.fmri.data.dims(funcdata)[5]) {
					timebyvox[tm,] = as.vector(funcvolume[,,,tm])[roi[[blob]]]
				}
				
				#NN = matrix(.fmri.data.datavec(avgdata)[roi[[blob]]],,1)%*%matrix(.fmri.data.datavec(avgdata)[roi[[blob]]],1,)
				NN = t(timebyvox)%*%timebyvox
				eigenvec[[blob]] = eigen(NN)$vectors[,1]
				
			}
		}
		
		
	}
	
	#calculate beta time-series estimates
	for(blob in 1:.fmri.data.dims(roidata)[5]) {
		
		if(type=='avg') {
			for(tm in 1:.fmri.data.dims(funcdata)[5]) b[blob,tm] = mean((as.vector(funcvolume[,,,tm])[roi[[blob]]]))
		}
		
		if(type=='ev') {
			if(evmodel=='spatiotemporal') {
				for(tm in 1:.fmri.data.dims(funcdata)[5]) b[blob,tm] = mean(eigenvec[[blob]][tm,])
			}
			
			if(evmodel=='spatial') {
				for(tm in 1:.fmri.data.dims(funcdata)[5]) b[blob,tm] = mean((as.vector(funcvolume[,,,tm])[roi[[blob]]])*eigenvec[[blob]])
			}
			
		}
		
	}
	
	#make correlation matrix and return matrices
	cmat <- cor.test.matrix(t(b),alpha=.05,bonf=F) 
	out <- list(ts=b,cor=cmat$cor,p=cmat$p)
	attr(out,'class') <- 'arfcorrelation'
	
	return(out)
}

makeRawTimeseries <- 
function(arfmodel,sefilename='single_events')
#make single trial estimates
{
	#get Header info from avgdatfile
	headinf <- readHeader(getFileInfo(.model.avgdatfile(arfmodel)))
	
	#open functional file
	filelist <- .model.betafiles(arfmodel)
	path <- .model.funcDir(arfmodel)
	sp <- .Platform$file.sep
	
	datavec = rawvec = numeric(0)
	totdim = 0
	
	for(filename in filelist) {
		if(file.exists(filename)) {
			
			info <- getFileInfo(filename)
			dirname <- .nifti.fileinfo.filename(info)
			
			func <- loadRda(paste(path,sp,dirname,sp,.data.funcRda(arfmodel),sep=''))
			timings <- .functional.timings(func)
			
			#make array of fmri volume
			fmrivolume = readData(paste(.functional.fullpath(func),sp,.functional.functionaldata(func),sep=''))
			funcvolume = .fmri.data.datavec(fmrivolume)
			dim(funcvolume) = c(.fmri.data.dims(fmrivolume)[2],.fmri.data.dims(fmrivolume)[3],.fmri.data.dims(fmrivolume)[4],.fmri.data.dims(fmrivolume)[5])
			n = .fmri.data.dims(fmrivolume)[2]*.fmri.data.dims(fmrivolume)[3]*.fmri.data.dims(fmrivolume)[4]
			
			#create timeseries in seconds and create double gamma
			tslen = round(.fmri.data.dims(fmrivolume)[5] * .fmri.data.pixdim(fmrivolume)[5])
			stick = rep(0,tslen)
			hrf <- gamma.fmri(1:tslen) 
			stick[round(timings)]=1
			vecnums = which(stick==1)
			
			#define design matrices and outputs
			X = matrix(NA,.fmri.data.dims(fmrivolume)[5],length(vecnums))
			beta = matrix(0,n,length(vecnums))
			raw = matrix(0,n,length(seq(1,tslen,.fmri.data.pixdim(fmrivolume)[5])))
			
			#create single-trial columns in design matrix
			for(i in 1:length(vecnums)) {
				st_protocol = rep(0,tslen)
				st_protocol[vecnums[i]]=1
				bold = convol.fmri(hrf,st_protocol)
				bold = bold[seq(1,tslen,.fmri.data.pixdim(fmrivolume)[5])]
				X[,i] = bold 
			}
			
			#Solve the LS equation for each voxel (on DEMEANED data)
			Xp = solve(t(X)%*%X)%*%t(X)
			
			i=1;
			for(z in 1:.fmri.data.dims(fmrivolume)[4]) {
				for(y in 1:.fmri.data.dims(fmrivolume)[3]) {
					for(x in 1:.fmri.data.dims(fmrivolume)[2]) {
						if(.model.mask(arfmodel)[i]==1) {
							dat = as.vector(funcvolume[x,y,z,])
							dat = dat-mean(dat)
							beta[i,] = as.vector(Xp%*%dat)
						}
						i=i+1;
					}
				}
			}
			
			
			
			i=1;
			for(z in 1:.fmri.data.dims(fmrivolume)[4]) {
				for(y in 1:.fmri.data.dims(fmrivolume)[3]) {
					for(x in 1:.fmri.data.dims(fmrivolume)[2]) {
						if(.model.mask(arfmodel)[i]==1) {
							
							betas = stick
							betas[which(betas==1)] = beta[i,]
							
							bold = convol.fmri(hrf,betas)
							bold = bold[seq(1,tslen,.fmri.data.pixdim(fmrivolume)[5])]
														
							dat = as.vector(funcvolume[x,y,z,])
							dat = dat-mean(dat)
						
							raw[i,] = dat-bold
						}
						i=i+1;
					}
				}
			}
			
			cat('finished trial\n')
			
			#concatenate datavecs and increase dimensions of volume
			rawvec = c(rawvec,as.vector(raw))
			
			totdim = totdim + length(vecnums)
			
		}
	}
		
	rawvolume = fmrivolume
	
	#make new file for filetered single events
	.fmri.data.filename(rawvolume) = paste('rawseries_',sefilename,sep='')
	.fmri.data.dims(rawvolume)[5] = totdim
	.fmri.data.pixdim(rawvolume)[5] = 1
	.fmri.data.datavec(rawvolume) = rawvec
	.fmri.data.fullpath(rawvolume) = .model.funcDir(arfmodel)
	writeData(rawvolume,.fmri.data.datavec(rawvolume))
		
	
	return(rawvolume)
	
	
}


