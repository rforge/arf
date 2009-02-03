# METHOD DEFINITIONS
# Wouter D. Weeda, University of Amsterdam
###############################################################################

## SETTINGS methods
 # show
setMethod('show','settings',
		function(object) {
			cat('[ARF SETTINGS]\n\n')				
			cat('[min]\n')
			cat(' iteration limit:',object@min.iterlim,'\n\n')
			cat('[start]\n')
			cat(' method:',object@start.method,'\n')
			cat(' fwhm:',object@start.maxfac,'voxels\n')
			cat(' values:',object@start.vector,'\n\n')
			cat('[rng]\n')
			cat(' method:',object@chk.method,'\n')
			cat(' ranges:',object@chk.range,'\n\n')
			cat('[sw]\n')
			cat(' type:',object@sw.type,'\n\n')
			
		}
)

#setMethod('show','data',
#	function(object) {
#		cat('[ARF DATA]\n\n')
#		cat('',object@name,'\n')
#		cat('',object@fullpath,'\n')
#		print(object@datafiles)
#		print(object@weightfiles)
#		cat('',object@avgdatfile,'\n')
#		cat('',object@avgWfile,'\n')
#		cat('',object@trials,'\n\n')
#	}		
#)

