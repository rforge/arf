# Plot and Misc functions for eye-tracking heatmap analysis
# W.D. Weeda, University of Amsterdam

###############################################################################



heatplot <- function(dat,title='') {
	
	hmcols = colorRampPalette(c("blue","green","yellow","red"))(256)
	
	image(1:dim(dat)[2],1:dim(dat)[1],t(dat),col=hmcols,xlab='X location',ylab='Y location',main=title)

}

compareheat <- function(dat1,dat2,thres=1e-06,title='') {
	
	hmcols1 = colorRampPalette(c("blue","yellow","red"))(3)
	#hmcols2 = colorRampPalette(c("red","yellow"))(256)
	
	dat1[dat1<thres]=0
	dat1[dat1>thres]=1
	
	dat2[dat2<thres]=0
	dat2[dat2>thres]=1
	
	dat = dat1 + dat2
	
	
	image(1:dim(dat)[2],1:dim(dat)[1],t(dat),col=hmcols1,xlab='X location',ylab='Y location',main=title)
	
	
}

perspcolors <- function(x, col=colorRampPalette(c("blue","green","yellow","red"))(256)) {
	
	x2 = (x[-1,-1] + x[-1,-(ncol(x)-1)] + x[-(nrow(x)-1),-1] + x[-(nrow(x)-1), -(ncol(x)-1)])/4
	
	cols = col[cut(x2,breaks=length(col),include.lowest=TRUE)]
	
	return(cols)
}