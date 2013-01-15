# Plot and Misc functions for eye-tracking heatmap analysis
# W.D. Weeda, University of Amsterdam

###############################################################################



heatplot <- function(dat) {
	
	hmcols = colorRampPalette(c("blue","green","yellow","red"))(256)
	
	image(1:dim(dat)[2],1:dim(dat)[1],t(dat),col=hmcols)

}