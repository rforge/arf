#############################################
# arf3DS4 S4 CLASS METHODS					#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#setGenerics for functions
setGeneric('plot')
setGeneric('summary')


setMethod('show','experiment',
		function(object) {
			
			cat('** arf experiment **\n')
			cat('name:      ',toupper(object@name),'\n',sep='')
			cat('path:      ',object@path,'\n',sep='')
			cat('subjects[',object@subject.num,']\n',sep='')
			for(subs in object@subject.names) cat('  - ',subs,'\n',sep='')
			cat('conditions[',object@condition.num,']\n',sep='')
			for(conds in object@condition.names) cat('  - ',conds,'\n',sep='')
			cat('\n')

		}
)

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

setMethod('summary','fmri.data',
	function(object) {
		cat(object@descrip,'\n')

	}		
		
		
)
setMethod('show','data',
		function(object) {
			cat('** arf data **\n')
			cat('name:          ',toupper(object@name),'\n')
			cat('path:          ',object@fullpath,'\n')
			cat('betafiles:     ',length(object@betafiles),'\n')
			cat('weightfiles:   ',length(object@betafiles),'\n')
			cat('avgbetafile:   ')
			if(length(object@avgdatfile)>0)	if(!file.exists(object@avgdatfile)) cat('not available\n') else cat('exists\n')
			cat('avgweightfile: ')
			if(length(object@avgWfile)>0) if(!file.exists(object@avgWfile)) cat('not available\n') else cat('exists\n')
			
		}
)

