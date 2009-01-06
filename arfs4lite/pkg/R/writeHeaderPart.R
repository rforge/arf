`writeHeaderPart` <-
function(con,headinf) {
	## writeHeaderPart writes nifti/analyze headerfiles
	## input is connection,headerinfo,and datavec
	## output is connection
			
	#if connection is open try and read header info (magicstring and headersize)
	if(con) {
		#write all elements (independent of NIFTI or ANALYZE)                       		  		  	  				#byte offset, type, descr.
		writeBin(as.integer(.nifti.header.sizeof_hdr(headinf)),con,size=4,endian=.nifti.header.endian(headinf))		#  0, int32,  MUST BE 348
		writeBin(charToRaw(.nifti.header.data_type(headinf)),con,endian=.nifti.header.endian(headinf))                 #  4, char[10]
		writeBin(charToRaw(.nifti.header.db_name(headinf)),con,endian=.nifti.header.endian(headinf))                   # 14, char[18]
		writeBin(as.integer(.nifti.header.extents(headinf)),con,size=4,endian=.nifti.header.endian(headinf))           # 32, int32
		writeBin(as.integer(.nifti.header.session_error(headinf)),con,size=2,endian=.nifti.header.endian(headinf))     # 36, short
		writeBin(charToRaw(.nifti.header.regular(headinf)),con,endian=.nifti.header.endian(headinf))                   # 38, char[1]
		writeBin(charToRaw(.nifti.header.dim_info(headinf)),con,endian=.nifti.header.endian(headinf))                  # 39, char[1], MRI SLICE ORDERING
		writeBin(as.integer(.nifti.header.dims(headinf)),con,size=2,endian=.nifti.header.endian(headinf))              # 40, short[8], DATA ARRAY DIMENSIONS
		writeBin(as.double(.nifti.header.intent_p1(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          # 56, float, 1ST INTENT PARAMETER
		writeBin(as.double(.nifti.header.intent_p2(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          # 60, float, 2ND INTENT PARAMETER
		writeBin(as.double(.nifti.header.intent_p3(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          # 64, float, 3RD INTENT PARAMETER
		writeBin(as.integer(.nifti.header.intent_code(headinf)),con,size=2,endian=.nifti.header.endian(headinf))       # 68, short, NIFITINTENT CODE
		writeBin(as.integer(.nifti.header.datatype(headinf)),con,size=2,endian=.nifti.header.endian(headinf))          # 70, short, DEFINES DATA TYPE
		writeBin(as.integer(.nifti.header.bitpix(headinf)),con,size=2,endian=.nifti.header.endian(headinf))            # 72, short, NUMBER BITS PER VOXEL
		writeBin(as.integer(.nifti.header.slice_start(headinf)),con,size=2,endian=.nifti.header.endian(headinf))       # 74, short, FIRST SLICE INDEX
		writeBin(as.double(.nifti.header.pixdim(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             # 76, float[8], GRID SPACINGS
		writeBin(as.double(.nifti.header.vox_offset(headinf)),con,size=4,endian=.nifti.header.endian(headinf))         #108, float, OFFSET INTO .NII FILE
		writeBin(as.double(.nifti.header.scl_slope(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #112, float, DATA SCALING: SLOPE
		writeBin(as.double(.nifti.header.scl_inter(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #116, float, DATA SCALING: INTERCEPT
		writeBin(as.integer(.nifti.header.slice_end (headinf)),con,size=2,endian=.nifti.header.endian(headinf))        #120, short, LAST SLICE INDEX
		writeBin(as.integer(.nifti.header.slice_code(headinf)),con,size=1,endian=.nifti.header.endian(headinf))        #122, integer[1], SLICE TIMING ORDER
		writeBin(as.integer(.nifti.header.xyzt_units(headinf)),con,size=1,endian=.nifti.header.endian(headinf))        #123, integer[1], UNITS OF PIXDIM
		writeBin(as.double(.nifti.header.cal_max(headinf)),con,size=4,endian=.nifti.header.endian(headinf))            #124, float, MAX DISPLAY INTENSITY
		writeBin(as.double(.nifti.header.cal_min(headinf)),con,size=4,endian=.nifti.header.endian(headinf))            #128, float, MIN DISPLAY INTENSITY
		writeBin(as.double(.nifti.header.slice_duration(headinf)),con,size=4,endian=.nifti.header.endian(headinf))     #132, float, TIME FOR 1 SLICE
		writeBin(as.double(.nifti.header.toffset(headinf)),con,size=4,endian=.nifti.header.endian(headinf))            #136, float, TIME AXIS SHIFT
		writeBin(as.integer(.nifti.header.glmax(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             #140, int32
		writeBin(as.integer(.nifti.header.glmin(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             #144, int32
		writeBin(charToRaw(.nifti.header.descrip(headinf)),con,endian=.nifti.header.endian(headinf))                   #148, char[80], TEXT
		writeBin(charToRaw(.nifti.header.aux_file(headinf)),con,endian=.nifti.header.endian(headinf))                  #228, char[24], AUXILIARY FILENAME
		writeBin(as.integer(.nifti.header.qform_code(headinf)),con,size=2,endian=.nifti.header.endian(headinf))        #252, short, NIFITXFORM CODE
		writeBin(as.integer(.nifti.header.sform_code(headinf)),con,size=2,endian=.nifti.header.endian(headinf))        #254, short, NIFITXFORM CODE
		writeBin(as.double(.nifti.header.quatern_b(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #256, float, QUATERNION B PARAM
		writeBin(as.double(.nifti.header.quatern_c(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #260, float, QUATERNION C PARAM
		writeBin(as.double(.nifti.header.quatern_d(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #264, float, QUATERNION D PARAM
		writeBin(as.double(.nifti.header.qoffset_x(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #268, float, QUATERNION X SHIFT
		writeBin(as.double(.nifti.header.qoffset_y(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #272, float, QUATERNION Y SHIFT
		writeBin(as.double(.nifti.header.qoffset_z(headinf)),con,size=4,endian=.nifti.header.endian(headinf))          #276, float, QUATERNION Z SHIFT
		writeBin(as.double(.nifti.header.srow_x(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             #280, float[4], 1ST ROW AFFINE TRANSFORM
		writeBin(as.double(.nifti.header.srow_y(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             #296, float[4], 2ND ROW AFFINE TRANSFORM
		writeBin(as.double(.nifti.header.srow_z(headinf)),con,size=4,endian=.nifti.header.endian(headinf))             #312, float[4], 3RD ROW AFFINE TRANSFORM
		writeBin(charToRaw(.nifti.header.intent_name(headinf)),con,endian=.nifti.header.endian(headinf))               #328, char[16], NAME OR MEANING OF DATA
		writeBin(charToRaw(.nifti.header.magic(headinf)),con,endian=.nifti.header.endian(headinf))      	
		if(.nifti.header.vox_offset(headinf)>348) for(i in 1:(.nifti.header.vox_offset(headinf)-348))  writeBin(as.integer(0),size=1,con,endian=.nifti.header.endian(headinf))
	
		
	} else {
		stop('Unable to open connection.\n')
	}
	
	#return nifti.header object
	return(con)
	
	
}

