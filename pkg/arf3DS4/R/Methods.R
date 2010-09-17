#############################################
# arf3DS4 S4 CLASS METHODS					#
# Copyright(c) 2009 Wouter D. Weeda			#
# University of Amsterdam					#
#############################################

#setGenerics for functions
setGeneric('plot',package='graphics')
setGeneric('summary',package='base')


#### EXPERIMENT METHODS ####
setMethod('show','experiment',
	function(object) {
		
		cat('[ ARF experiment ]\n')
		cat('name:      ',toupper(object@name),'\n',sep='')
		cat('path:      ',object@path,'\n',sep='')
		cat('\n')
		cat('subjects[',object@subject.num,']\n',sep='')
		for(subs in object@subject.names) cat('  > ',subs,'\n',sep='')
		cat('\n')
		cat('conditions[',object@condition.num,']\n',sep='')
		for(conds in object@condition.names) cat('  > ',conds,'\n',sep='')
		cat('\n')

	}
)

#### FMRI DATA METHODS ####
setMethod('plot',signature(x='fmri.data',y='missing'),
	function(x,y,zerotol=1e-3,what=c('all','pos','neg'),col=c('rgb','gray'),volume=1,slices=1:x@dims[4],max.asp=NULL,device=NULL,...) {
		
		dimx <- x@dims[2]
		dimy <- x@dims[3]
		dimz <- x@dims[4]
		
		if(x@dims[1]==4) data <- x@datavec[(1:(dimx*dimy*dimz))+(dimx*dimy*dimz)*(volume-1)] else data <- x@datavec[1:(dimx*dimy*dimz)]
		
		dim(data) <- c(dimx,dimy,dimz)
		
		if(reqFlip(x)[1]) data <- flipAxis(data,'x')
		if(reqFlip(x)[2]) data <- flipAxis(data,'y')
		if(reqFlip(x)[3]) data <- flipAxis(data,'z')
		
		#make layout of slices
		numslices=length(slices)
		m <- round(sqrt(numslices+1)+.5)
		if(numslices==1) m=1
		
		if(!is.null(device)) eval(parse(text=device))
		
		layout(matrix(1:m^2,m,m,byrow=T))
		par(mar=c(2,2,1,1),las=1)
		
		what = match.arg(what)
		col = match.arg(col)
		
		asp = dimy/dimx
		if(!is.null(max.asp)) asp=max.asp
		
		if(what=='pos') data[data<0]=0
		if(what=='neg') data[data>0]=0
		if(col=='gray') gray = TRUE else gray=FALSE
		
		newdata = makeDiscreteImage(as.vector(data),zerotol=zerotol)
		colors = makeColors(newdata,gray)
		dim(newdata) <- c(dimx,dimy,dimz)	
	
		#fill slices
		for(i in slices) {
			colvec = sliceColor(as.vector(newdata[,,i]),colors)
			
			unvec = unique(as.vector(newdata[,,i]))
			if(length(unvec)<=2) {
				if(any(unvec<0)) colvec=colors$colvec
				if(any(unvec>0)) colvec=colors$colvec
			}  
				
			par(mgp=c(1.1,0,0))
			image(1:dimx,1:dimy,newdata[,,i],bty='n',main='',axes=F,col=colvec,xlab='',ylab='',asp=asp)
			axis(1,at=round(dimx/2),labels='P',tick=F)
			axis(2,at=round(dimy/2),labels='R',tick=F)
			axis(3,at=c(1,round(dimx/2)),labels=c(i,'A'),tick=F)
			axis(4,at=round(dimy/2),labels='L',tick=F)
			
		}
		
		#only add if numslices is > 1
		if(numslices>1) {
			par(las=1,mar=c(2, 2, 1, 1) + 0.1,mgp=c(3,1,0))
			image(x=c(1),y=colors$data,z=matrix(colors$data,1),axes=F,col=colors$colvec,xlab='',ylab='',asp=asp)
			axis(2,at=c(min(colors$data),0,max(colors$data)),labels=c(round(min(data),2),0,round(max(data),2)),cex=1.5)
		
			if(((m*m-numslices)-1)>0) for(i in 1:((m*m-numslices)-1)) plot(NA,NA,xlim=c(0,1),ylim=c(0,1),bty='n',axes=F,xlab='',ylab='')			
		}
		
		#set layout to one again
		#and close graphics devices
		if(names(dev.cur())!='X11') if(names(dev.cur())!='quartz') dev.off()
		layout(1)	
	}		
)

setMethod('show','fmri.data',
		function(object) {
			
			cat('[ ',object@filename,' ]\n\n')
			
			cat('<location>\n')
			cat('path:           ',object@fullpath,'\n')			
			cat('filename:       ',object@filename,'\n')			
			cat('extension:       ',object@extension,sep='')
			if(object@gzipped) cat('.gz\n') else cat('\n')
			cat('\n')
			cat('<file>\n')
			cat('filetype:       ',object@filetype,'\n')			#size of header (must be 348))
			cat('size of header: ',object@sizeof_hdr,'\n')			#size of header (must be 348))
			cat('type of data:   ',object@datatype,'\n')			#storage data type
			cat('bits_per_pixel: ',object@bitpix,'\n') 			#bits per pixel
			cat('magic string:   ',object@magic,'\n')
			cat('voxel offset:   ',object@vox_offset,'\n') 		#offset of data in .nii file
			cat('data.type:      ',object@data.type,'\n')		#type of data
			cat('signed:         ',object@data.signed,'\n')		#signed data
			cat('\n')
			cat('<fmri data>\n')
			cat('description:    ',object@descrip,'\n') 
			#cat('aux file:       ',object@aux_file,'\n') 
			cat('intent code:    ',object@intent_code,'\n') 
			#cat('intent p1:      ',object@intent_p1,'\n') 
			#cat('intent p2:      ',object@intent_p2,'\n') 
			#cat('intent p3:      ',object@intent_p3,'\n') 
			cat('intentname:     ',object@intent_name,'\n')		#meaning of data
			#cat('data type:      ',object@data_type,'\n')		
			#cat('db name:        ',object@db_name,'\n')			
			cat('\n')
			cat('<dimensions>\n')
			#cat('dim_info:       ',object@dim_info,'\n')
			cat('xyzt units:     ',xyzt2char(object@xyzt_units),'\n') 
			cat('pixel dims:     ',object@pixdim,'\n') 			#voxel dimensions
			cat('dimensions:     ',object@dims,'\n')
			cat('q form code:    ',object@qform_code,'\n') 
			cat('s form code:    ',object@sform_code ,'\n')
			#cat('quaternion b:   ',object@quatern_b,'\n') 
			#cat('quaternion c:   ',object@quatern_c,'\n')
			#cat('quaternion d:   ',object@quatern_d,'\n') 
			#cat('q offset x:     ',object@qoffset_x,'\n') 
			#cat('q offset y:     ',object@qoffset_y,'\n') 
			#cat('q offset z:     ',object@qoffset_z,'\n')
			#cat('s row x:        ',object@srow_x,'\n') 
			#cat('s row y:        ',object@srow_y,'\n')
			#cat('s row z:        ',object@srow_z,'\n')
			#cat('max display:    ',object@cal_max,'\n') 
			#cat('min display:    ',object@cal_min,'\n') 
			#cat('\n')
			#cat('** scanner info **\n')
			#cat('slice start:    ',object@slice_start,'\n') 	
			#cat('slice duration: ',object@slice_duration,'\n') 
			#cat('slice end:      ',object@slice_end ,'\n')
			#cat('slice timing:   ',object@slice_code,'\n') 
			#cat('time offset:    ',object@toffset,'\n') 
			#cat('scale slope:    ',object@scl_slope,'\n')
			#cat('scale offset:   ',object@scl_inter,'\n') 
			#cat('extents:        ',object@extents,'\n')			
			#cat('sessior_error:  ',object@session_error,'\n')
			#cat('regular:        ',object@regular,'\n')
			#cat('gl max:         ',object@glmax,'\n') 
			#cat('gl min:         ',object@glmin,'\n') 
			cat('\n')
		}
)

setMethod('summary','fmri.data',
		function(object) {
			
			cat('[ ',object@filename,' ]\n')
			cat('type:        ',object@filetype,'\n',sep='')
			cat('dims:		 ')
			for(i in 2:(object@dims[1]+1)) cat(object@dims[i],' ')
			cat('\n')
			cat('description: ',object@descrip,'\n',sep='')
			cat('\n')
			
			zeroes = object@datavec[object@datavec==0]
			if(length(zeroes)>0) object@datavec = object@datavec[-which(object@datavec==0)]
			
			qs = quantile(object@datavec,c(.25,.5,.75))
			iqr = IQR(object@datavec)
			
			cat('minimum:   ',sprintf('%9.4f',min(object@datavec)),'\n')
			cat('maximum:   ',sprintf('%9.4f',max(object@datavec)),'\n')
			cat('mean:      ',sprintf('%9.4f',mean(object@datavec,na.rm=T)),'\n')
			cat('1st quart: ',sprintf('%9.4f',qs[1]),'\n')
			cat('median:    ',sprintf('%9.4f',qs[2]),'\n')
			cat('3rd quart: ',sprintf('%9.4f',qs[3]),'\n')
			cat('iqr:       ',sprintf('%9.4f',iqr),'\n')
			cat('\n')
			
		}
)

setMethod('[','fmri.data',
		function(x,i,j,k,l,drop='missing') {
			if(x@dims[1]>3) dat = array(x@datavec,dim=c(x@dims[2],x@dims[3],x@dims[4],x@dims[5])) else dat = array(x@datavec,dim=c(x@dims[2],x@dims[3],x@dims[4]))
		
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			if(missing(k)) k = NULL
			if(missing(l)) l = NULL
			
			if(x@dims[1]>3) out = eval(parse(text=paste('dat[',i,',',j,',',k,',',l,']',sep=''))) else out = eval(parse(text=paste('dat[',i,',',j,',',k,']',sep='')))
				
			return(out)
		}
)

setMethod('[<-','fmri.data',
		function(x,i,j,k,l,value,drop='missing') {
			if(x@dims[1]>3) dat = array(x@datavec,dim=c(x@dims[2],x@dims[3],x@dims[4],x@dims[5])) else dat = array(x@datavec,dim=c(x@dims[2],x@dims[3],x@dims[4]))
			
			if(missing(i)) i = NULL
			if(missing(j)) j = NULL
			if(missing(k)) k = NULL
			
			if(x@dims[1]>3) {
				if(missing(l)) l = NULL
			} else value = l
			
			if(x@dims[1]>3) out = eval(parse(text=paste('dat[',i,',',j,',',k,',',l,']<-value',sep=''))) else out = eval(parse(text=paste('dat[',i,',',j,',',k,']<-value',sep='')))
			
			x@datavec = as.vector(dat) 
					
			return(x)
		}
)

#### ARF MODEL METHODS ####
setMethod('plot',signature(x='model',y='missing'),
		function(x,y,...) {
			
			fn = paste(x@modeldatapath,.Platform$file.sep,x@fullmodelDataFile,sep='')
			
			mod = try(readData(fn),silen=T)
			
			if(class(mod)=='try-error') {
				mod = new('fmri.data',x@dataHeader)
				if(length(x@estimates)==x@regions*x@params & is.numeric(x@estimates)) {
					mod@datavec = model.gauss(x@estimates,x@regions*x@params,x@dataHeader@dims[2],x@dataHeader@dims[3],x@dataHeader@dims[4])
					plot(mod,...)
				}
			} else {
				plot(mod,...)	
			}
		}

)
setMethod('show','model',
		function(object) {
			cat(paste('[ ARF ',tolower(object@modelname),' ]\n',sep=''))
			cat(' regions: ',object@regions,'\n')
			cat(' valid:   ',object@valid,'\n')
			cat(' warnings:\n')
			for(warns in object@warnings) cat('  ',warns,'\n')
			cat('\n')
			if(object@valid==T) {
				cat(' modelinfo:\n')
				cat(' ',object@convergence,'\n')
				cat('  fit (BIC,RMSEA):',round(object@fit[1]),round(object@fit[2],1),'\n')
				cat('  minimum:   ',object@minimum,'\n')
				cat('  estimates:\n')
				if(length(object@wald@pvalues)==0) {object@wald@pvalues=matrix(1,object@regions,5);w=FALSE} else w=TRUE
				
				for(reg in 1:object@regions) {
					cat('  ',sprintf('[%3d]  (%3.0f,%3.0f,%3.0f)',reg,object@estimates[1+(10*(reg-1))],object@estimates[2+(10*(reg-1))],object@estimates[3+(10*(reg-1))]))
					if(object@wald@pvalues[reg,4]<.05) {	
						cat(' ',sprintf('[%5.1f %5.1f %5.1f ~ %5.1f %5.1f %5.1f]* ',object@estimates[4+(10*(reg-1))],object@estimates[5+(10*(reg-1))],object@estimates[6+(10*(reg-1))],object@estimates[7+(10*(reg-1))],object@estimates[8+(10*(reg-1))],object@estimates[9+(10*(reg-1))]))
					} else cat(' ',sprintf('[%5.1f %5.1f %5.1f ~ %5.1f %5.1f %5.1f]  ',object@estimates[4+(10*(reg-1))],object@estimates[5+(10*(reg-1))],object@estimates[6+(10*(reg-1))],object@estimates[7+(10*(reg-1))],object@estimates[8+(10*(reg-1))],object@estimates[9+(10*(reg-1))]))
					if(object@wald@pvalues[reg,5]<.05) {	
						cat(' ',sprintf('[%7.0f]*',object@estimates[10+(10*(reg-1))]),'\n')
					} else cat(' ',sprintf('[%7.0f] ',object@estimates[10+(10*(reg-1))]),'\n')
				}
				cat('\n')
				if(w) cat('  * Wald tests significant at .05 (uncorrected for number of regions)\n')
				if(!w) cat('  Wald statistics not calculated\n')
				cat('\n')
			}
			
		}
)

#### ARF DATA METHODS ####
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


#### ARF MNAMES METHODS ####
setMethod('show','mnames',
		function(object) {
			cat('experiment:',.experiment.name(.mnames.experiment(object)),'\n')
			cat('   subject:',.mnames.subject(object),'\n')
			cat(' condition:',.mnames.condition(object),'\n')
			cat('modelnames: [1]',.mnames.mnames(object)[1],'\n')
			if(length(.mnames.mnames(object))>1) {
				for(i in 2:length(.mnames.mnames(object))) {
			cat('            [',i,'] ',.mnames.mnames(object)[i],'\n',sep='')
				}
			}
		}
)




