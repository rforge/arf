#############################################
# arf3DS4 S4 DISPLAY FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#makeColors
#sliceColor
#makeDiscreteImage
#reqFlip

makeDiscreteImage <-
function(datavec,zerotol=1e-03)
#make a discritezed image of a datavector (divide into steps relative to zero-point)
{
	#define maxsteps
	maxsteps = 64
	
	datavec[abs(datavec)<zerotol]=0
		
	max_dat = max(datavec)
	min_dat = min(datavec)
	total = abs(min_dat)+abs(max_dat)
	
	possteps = round(maxsteps*(abs(max_dat)/total))
	negsteps = round(maxsteps*(abs(min_dat)/total))
	
	pos_data = datavec[datavec>0]
	neg_data = datavec[datavec<0]
	
		
	if(max_dat>0 & min_dat<0) {
		pq = quantile(pos_data,probs=seq(0,1,1/possteps)[-1])
		nq = quantile(neg_data,probs=seq(1,0,-1/negsteps)[-1])
	}
	
	if(max_dat>0 & min_dat>=0) {
		possteps = maxsteps
		pq = quantile(pos_data,probs=seq(0,1,1/possteps)[-1])
		nq = numeric(0)
	}
	
	if(max_dat<=0 & min_dat<0) {
		negsteps = maxsteps
		nq = quantile(neg_data,probs=seq(1,0,-1/maxsteps)[-1])
		pq = numeric(0)
	}
	
	if(max_dat==0 & min_dat==0) {
		nq = numeric(0)
		pq = numeric(0)
	}
		
	newdata=rep(NA,length(datavec))
	
	if(length(pq)>0) newdata[datavec>0 & datavec<pq[1]]=1
	if(length(nq)>0) newdata[datavec<0 & datavec>nq[1]]=-1
	
	if(length(pq)>0) for(i in 1:possteps) newdata[datavec>=pq[i]]=i+1
	if(length(nq)>0) for(i in 1:negsteps) newdata[datavec<=nq[i]]=-i-1
	
	newdata[datavec==0]=0
	
	return(newdata)
	
}


makeColors <-
function(datavec,gray=FALSE)
## make colors for overlay images, input is a discretized image
{
	datasort = sort(unique(datavec))
	
	neg_dat = datasort[datasort<0]
	pos_dat = datasort[datasort>0]
	
	if(gray) {
		
		pos_col = gray(seq(0,1,1/length(pos_dat))[-1])
		neg_col = gray(seq(1,0,-1/length(neg_dat))[-length(seq(1,0,-1/length(neg_dat)))])
		zero_col = gray(0)
		
	
	} else {
				
		pos_col <- rgb(1,seq(0,1,1/length(pos_dat))[-1],0)
		neg_col <- rgb(seq(.5,0,-.5/length(neg_dat))[-1],seq(.5,0,-.5/length(neg_dat))[-1],1)
		zero_col <- rgb(0,0,0)
		
	}
	
	colvec <-c(neg_col,zero_col,pos_col) 
	
	neg = matrix(NA,2,length(neg_col))
	pos = matrix(NA,2,length(pos_col))
	
	neg[1,]=neg_dat
	neg[2,]=neg_col
	pos[1,]=pos_dat
	pos[2,]=pos_col
	
	return(list(pos=pos,neg=neg,colvec=colvec,data=c(neg_dat,0,pos_dat)))
	
}

sliceColor <-
function(slicedata,colors)
## calculate the colorvector for the discretized slice based on an makeColor object. 
{
	
	slice_max = max(slicedata)
	slice_min = min(slicedata)
	
	mp = which(as.numeric(colors$pos[1,])<slice_max)
	mn = which(as.numeric(colors$neg[1,])<=slice_min)
	
	colvec_pos = colors$pos[2,mp]
	colvec_neg = colors$neg[2,-mn]
	
	colvec=c(colvec_neg,rgb(0,0,0),colvec_pos)
		
	return(colvec)
	
}


reqFlip <-
function(fmridata)
#check if a flip is required for display purposes
{
	flip = c(T,F,F)
	
	if(.fmri.data.sform_code(fmridata)>0) {
		
		if(.fmri.data.srow_x(fmridata)[1]<0) flip[1]=F
		if(.fmri.data.srow_y(fmridata)[2]<0) flip[2]=T
		if(.fmri.data.srow_z(fmridata)[3]<0) flip[3]=T
		
		
	} #else warning('sform_code not set, assuming radiological orientation')
	
	return(flip)
	
}
