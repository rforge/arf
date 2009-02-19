#############################################
# arf3DS4 S4 CLASS METHODS					#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

setMethod('show','experiment',
		function(object) {
			cat('Name:      ',object@name,'\n')
			cat('Path:      ',object@path,'\n')
			cat('Subjects:  ',object@subject.num,'\n')
			for(subs in object@subject.names) cat('  -',subs,'\n')
			cat('Conditions:',object@condition.num,'\n')
			for(conds in object@condition.names) cat('  -',conds,'\n')
			cat('\n')

		}
)

setGeneric('plot')
setMethod('plot',signature(x='fmri.data',y='missing'),
	function(x,y,mrs=F) {
		
		dimx <- x@dims[2]
		dimy <- x@dims[3]
		dimz <- x@dims[4]
		
		data <- x@datavec[1:(dimx*dimy*dimz)]
		dim(data) <- c(dimx,dimy,dimz)
				
		m <- round(sqrt(dimz)+.5)
		layout(matrix(1:m^2,m,m,byrow=T))
		par(mar=c(2,2,1,1),las=1)
		
		for(i in 1:dimz) image(1:dimx,1:dimy,data[,,i],bty='n',main=paste('z=',i,sep=''),axes=mrs,col=gray(seq(0,1,.01)))
		for(i in 1:(m*m-dimz)) plot(NA,NA,xlim=c(0,1),ylim=c(0,1),bty='n',axes=F)			
				
	}		
)
setGeneric('summary')
setMethod('summary','fmri.data',
	function(object) {
		cat(object@descrip,'\n')

	}		
		
		
)