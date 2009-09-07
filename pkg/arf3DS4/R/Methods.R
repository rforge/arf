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
		
		cat('[ ARF experiment ]\n')
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
		

		for(i in 1:dimz) image(1:dimx,1:dimy,data[,,i],bty='n',main=paste('z=',i,sep=''),axes=mrs,col=heat.colors(64))
			
		for(i in 1:(m*m-dimz)) plot(NA,NA,xlim=c(0,1),ylim=c(0,1),bty='n',axes=F)			
				
	}		
)


setMethod('show','data',
		function(object) {
			cat('[ ARF data ]\n')
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


setMethod('show','model',
		function(object) {
			cat(paste('[ ARF ',tolower(object@modelname),' ]\n',sep=''))
			cat(' file:    ',object@modelDataFile,'\n')
			cat(' regions: ',object@regions,'\n')
			cat(' mintime: ',object@proctime[1],'\n')
			cat(' swctime: ',object@proctime[2],'\n')
			cat(' valid:   ',object@valid,'\n')
			
			cat(' warnings:\n')
			for(warns in object@warnings) cat('  ',warns,'\n')
			cat('\n')
			
			if(object@valid==T) {
				cat(' modelinfo:\n')
				cat(' ',object@convergence,'\n')
				cat('  fit (BIC,RMSEA):',object@fit,'\n')
				cat('  minimum:   ',object@minimum,'\n')
				cat('  estimates:\n')
				for(reg in 1:object@regions) {
					cat('  ',sprintf('[%3d]  (%3.0f,%3.0f,%3.0f)',reg,object@estimates[1+(10*(reg-1))],object@estimates[2+(10*(reg-1))],object@estimates[3+(10*(reg-1))]))
					cat(' ',sprintf('[%5.1f %5.1f %5.1f ~ %5.1f %5.1f %5.1f]',object@estimates[4+(10*(reg-1))],object@estimates[5+(10*(reg-1))],object@estimates[6+(10*(reg-1))],object@estimates[7+(10*(reg-1))],object@estimates[8+(10*(reg-1))],object@estimates[9+(10*(reg-1))]))
					cat(' ',sprintf('[%7.0f]',object@estimates[10+(10*(reg-1))]),'\n')	
				}
				
			}
			
		}
)