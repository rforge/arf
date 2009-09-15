#############################################
# arf3DS4 S4 DISPLAY FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#makeColors
#sliceColor
#makeDiscreteImage

makeDiscreteImage <-
function(datavec,steps=64,zerotol=1e-03)
{
	max_dat = max(datavec)
	min_dat = min(datavec)
	
	datavec[abs(datavec)<zerotol]=0
	
	pos_data = datavec[datavec>0]
	neg_data = datavec[datavec<0]
	zero_data = datavec[datavec==0]
	
	pq = quantile(pos_data,probs=seq(0,1,1/steps)[-1])
	nq = quantile(neg_data,probs=seq(1,0,-1/steps)[-1])
	
	newdata=rep(NA,length(datavec))
		
	newdata[datavec>0 & datavec<pq[1]]=1
	newdata[datavec<0 & datavec>nq[1]]=-1
	
	for(i in 1:steps) {
		newdata[datavec>=pq[i]]=i+1
		newdata[datavec<=nq[i]]=-i-1
	}

	newdata[datavec==0]=0
	
	return(newdata)
}


makeColors <-
function(datavec)
## make colors for overlay images
{
	
	datasort = sort(unique(datavec))
	
	neg_dat = datasort[datasort<0]
	pos_dat = datasort[datasort>0]
	
	pos_col <- rgb(1,seq(0,1,1/length(pos_dat))[-1],0)
	neg_col <- rgb(seq(.5,0,-.5/length(neg_dat))[-1],seq(.5,0,-.5/length(neg_dat))[-1],1)
	zero_col <- rgb(0,0,0)
	
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
## calculate the colorvector for the slice
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