#############################################
# arf3DS4 S4 DISPLAY FUNCTIONS				#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#[CONTAINS]
#makeColors
#sliceColor
#makeDiscreteImage
#newprogressWindow

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




newProgressWindow <-
function(arfmodel)
#make a new Progress Window, return an object of class progress (S3)
{
	
	tt <- tktoplevel()
	mt = .model.modeltype(arfmodel)
	nr = .model.regions(arfmodel)
	tktitle(tt) <- paste('ARF Progress [ ',mt,' @ ',nr,' ]',sep='')
	
	#create heading
	text.heading <- tclVar(paste(.model.name(arfmodel),'process started',as.character(Sys.time())))
	label.heading <- tklabel(tt,width='50')
	tkconfigure(label.heading,textvariable=text.heading)
		
	#create shared comps
	text.1 <- tclVar('value')
	label.1 <- tklabel(tt,width=12)
	tkconfigure(label.1,textvariable=text.1)
	
	text.2 <- tclVar('iterate')
	label.2 <- tklabel(tt,width=12)
	tkconfigure(label.2,textvariable=text.2)
	
	text.3 <- tclVar('decrease')
	label.3 <- tklabel(tt,width=12)
	tkconfigure(label.3,textvariable=text.3)
	
	text.4 <- tclVar('norm')
	label.4 <- tklabel(tt,width=12)
	tkconfigure(label.4,textvariable=text.4)
	
	#create ssq
	text.ssq <- tclVar('objective')
	label.ssq <- tklabel(tt,width=12)
	tkconfigure(label.ssq,textvariable=text.ssq)
	
	text.ssq.val <- tclVar('0')
	label.ssq.val <- tklabel(tt,width=12)
	tkconfigure(label.ssq.val,textvariable=text.ssq.val)
	
	text.ssq.it <- tclVar('0')
	label.ssq.it <- tklabel(tt,width=12)
	tkconfigure(label.ssq.it,textvariable=text.ssq.it)
		
	#create gradient
	text.grad <- tclVar('gradient')
	label.grad <- tklabel(tt,width=12)
	tkconfigure(label.grad,textvariable=text.grad)
	
	text.grad.val <- tclVar('0')
	label.grad.val <- tklabel(tt,width=12)
	tkconfigure(label.grad.val,textvariable=text.grad.val)
	
	text.grad.it <- tclVar('0')
	label.grad.it <- tklabel(tt,width=12)
	tkconfigure(label.grad.it,textvariable=text.grad.it)
	

	#place at grid
	tkgrid(label.heading,columnspan=3)
	tkgrid(label.ssq, label.1, label.ssq.val)
	tkgrid(label.2, label.ssq.it,columnspan=2)
	tkgrid(label.3, label.grad.val,columnspan=2)
	tkgrid(label.grad, label.4, label.grad.it)
	
	tkgrid.configure(label.ssq, label.grad, sticky='e')
	tkgrid.configure(label.2, label.4, sticky='e')
	tkgrid.configure(label.ssq.val, label.ssq.it, label.grad.val, label.grad.it, sticky='w')
	
	#make progress object (S3)
	progress = list(ssq.val.tkobj=text.ssq.val,ssq.it.tkobj=text.ssq.it,grad.val.tkobj=text.grad.val,grad.it.tkobj=text.grad.it)
	attr(progress,'class') <- 'progress'
	
	#assign global counters
	assign('.gradit',0,envir=.GlobalEnv)
	assign('.objit',1,envir=.GlobalEnv)
	
	return(progress)
	
}

