`writeDataPart` <-
function(con,headinf,datavec) {
	## writeDataPart writes nifti/analyze datafiles
	## input is connection,headerinfo,and datavec
	## output is connection
			
	#write data to connection
	if(con) {
		mode(datavec) <- .nifti.header.data.type(headinf)	
		writeBin(datavec,con,size=(.nifti.header.bitpix(headinf)/8))
	} else stop('Unable to open connection') 

	
	#return TRUE
	return(con)

}

