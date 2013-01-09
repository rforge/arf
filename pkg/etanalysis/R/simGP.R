#simTools 


ellipse <- function(dimx,dimy,sdx,sdy,duration,conf.int=95) 
#Simulate ellipse (CI of normaldist)
{
	#set confidence interval and Chi-square threshold
	CI = (conf.int/100)
	Chi = qchisq(CI,3)
		
	#read dat, set dims, and create new 0-filled object
	if(dimx%%2==0) dimx=dimx+1
	if(dimy%%2==0) dimy=dimy+1
	dimz=1		
		
	#read in estimates in a matrix
	ests = c((dimx-1)/2,(dimy-1)/2,1,sdx,sdy,1,0,0,0,1)
		
	#for each region set the confidence interval based on X'C-1X <= Chi3
	newdat = array(0,c(dimx,dimy,dimz))
		
	theta = ests
	Sigma = matrix(c(theta[4]^2,theta[4]*theta[5]*theta[7],theta[4]*theta[6]*theta[8],theta[4]*theta[5]*theta[7],theta[5]^2,theta[5]*theta[6]*theta[9],theta[4]*theta[6]*theta[8],theta[5]*theta[6]*theta[9],theta[6]^2),3)
	SI = solve(Sigma)
		
	for(z in 1:dimz) {
		for(y in 1:dimy) {
			for(x in 1:dimx) {
				X = c(x-theta[1],y-theta[2],z-theta[3]) 
				C = t(X)%*%SI%*%X
				if(C<=Chi) newdat[x,y,z] = 1
			}
		}
	}

	return(t(newdat[,,1]))
}


makeeye <- function()
#make an eye-shape
{
	eye = ellipse(20,14,3,2.2)
	eye = eye[-c(14:15),-c(1,(19:21))]
	
	return(eye)
	
}



simHM <- function(dimx,dimy,duration,mode=c('random','normfocus'),posxlim=NULL,posylim=NULL,control.norm=list(mean=c(50,50),sd=c(1,1))) 
{

	fourD = array(0,dim=c(dimy,dimx,duration))
	
	eye = makeeye()
	dm = dim(eye)
	
	if(is.null(posxlim)) posxlim=c(1,dimx-dm[2]-1)
	if(is.null(posylim)) posylim=c(1,dimy-dm[1]-1)
	
	for(i in 1:duration) 
	{
		
		if(mode=='random') {
			loc_x = sample(posxlim[1]:posxlim[2],1)
			loc_y = sample(posylim[1]:posylim[2],1)
		}
		
		if(mode=='normfocus') {
			loc_x = round(rnorm(1,control.norm$mean[1],control.norm$sd[1]))
			loc_y = round(rnorm(1,control.norm$mean[2],control.norm$sd[2]))
			
			if(loc_x<posxlim[1]) loc_x = posxlim[1]
			if(loc_x>posxlim[2]) loc_x = posxlim[2]
			
			if(loc_y<posylim[1]) loc_y = posylim[1]
			if(loc_y>posylim[2]) loc_y = posylim[2]
			
		}
		
		
		fourD[(loc_y:(loc_y+dm[1]-1)),(loc_x:(loc_x+dm[2]-1)),i]=eye
		
	}
	
	return(list(heatmap=apply(fourD,c(1,2),mean),rawdata=fourD))
	
}