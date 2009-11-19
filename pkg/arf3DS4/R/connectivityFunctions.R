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
	
	#demean b
	for(i in 1:length(regs)) b[i,]=b[i,]-mean(b[i,])
		

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
	
	#make new file for fileterd single events
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

tscor <-
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
	
	pcor = partialCor(cormat)
	
	delsig = which(is.na(siglist[,1]))
	if(length(delsig)>0) siglist = siglist[-delsig,]
	o = order(abs(siglist[,3]),decreasing=T)
	siglist = siglist[o,]
	
	corlist= list(cor=cormat,p=pmat,sig=sigmat,pcor=pcor,siglist=siglist)
	
	return(corlist)
}


processCorrelations <- 
function(tsdata,arfmodel,alpha=.05,bonf=T,pc=T,sort=c('euclid','correlation','pvalue','region')) 
#calculate interregion correlations (with Eucliddistances)
{
	
	corlist = cor.test.matrix(tsdata,alpha,bonf)
	
	siglist = corlist$siglist[,1:4]

	if(pc) {
		del=numeric(0)
		for(i in 1:nrow(siglist)) if(corlist$pcor[siglist[i,1],siglist[i,2]]<=0) del=c(del,i)
		if(length(del)>0) siglist = siglist[-del,]
	}
	
	eudist = euclidDist(arfmodel)
		
	ncor = nrow(siglist)
	loc1 = loc2 = matrix(NA,ncor,3)
	eud = matrix(NA,ncor,1)
	
	estmat = matrix(.model.estimates(arfmodel),10)
	
	for(i in 1:ncor) {
		loc1[i,] = round(estmat[c(1,2,3),siglist[i,1]])
		loc2[i,] = round(estmat[c(1,2,3),siglist[i,2]])
		eud[i,] = round(eudist[siglist[i,1],siglist[i,2]])
	}
	
	corlist$siglist = cbind(siglist,loc1,loc2,eud)
	colnames(corlist$siglist) =  c('region1','region2','estimate','p-value','r1x','r1y','r1z','r2x','r2y','r2z','euclid')
	
	sort = match.arg(sort)
	
	
	if(sort=='euclid') o = order(corlist$siglist[,11],decreasing=T)
	if(sort=='correlation') o = order(abs(corlist$siglist[,3]),decreasing=T)
	if(sort=='pvalue') o = order(corlist$siglist[,4],decreasing=F)
		
	if(sort=='region') {
		o = order(corlist$siglist[,1],decreasing=F)
		to = corlist$siglist[o,]
		
		for(i in unique(to[,1]))
		{
			to2 = as.matrix(to[which(to[,1]==i),])
			if(ncol(to2)>1) {
				o2 = order(to2[,2],decreasing=F)
				to[which(to[,1]==i),] =	to2[o2,]
			}
		}
		corlist$siglist = to
	}
	
	if(!sort=='region') corlist$siglist = corlist$siglist[o,]
	
	
	
	
	return(data.frame(corlist$siglist))
}


