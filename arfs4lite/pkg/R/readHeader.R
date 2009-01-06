`readHeader` <-
function(fileinf) {
	## readHeader reads nifti header info from nii,img,hdr and gzipped exts.
	## input is an object of class nifti.fileinfo (containing only file path, location, extension, gzippedness and endianness)
	## output is an object of class nifti.header (which inherits nifti.fileinfo and adds values from nifti header file)	
	
	#open new header headinf
	headinf=new('nifti.header',fileinf)
	
	#open based on gzippedness
	if(.nifti.header.gzipped(headinf)==TRUE) {
		con <- gzfile(paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',.nifti.header.extension(headinf),'.gz',sep=''),open='rb')	
	} else {
		con <- file(paste(.nifti.header.fullpath(headinf),.nifti.header.filename(headinf),'.',.nifti.header.extension(headinf),sep=''),open='rb')		
	}
	
	#if connection is open try and read header info (magicstring and headersize)
	if(con) {
		#read in all elements (independent of NIFTI or ANALYZE)                       		  		  	  											#byte offset, type, descr.
		.nifti.header.sizeof_hdr(headinf) <- readBin(con,integer(),n=1,size=4,signed=T,endian=.nifti.header.endian(headinf))         #  0, int32,  MUST BE 348
		.nifti.header.data_type(headinf) <- rawToChar(readBin(con,raw(),n=10))                                		  								#  4, char[10]
		.nifti.header.db_name(headinf) <- rawToChar(readBin(con,raw(),n=18))                                  		  								# 14, char[18]
		.nifti.header.extents(headinf) <- readBin(con,integer(),n=1,size=4,signed=T,endian=.nifti.header.endian(headinf))            # 32, int32
		.nifti.header.session_error(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian =.nifti.header.endian(headinf))     # 36, short
		.nifti.header.regular(headinf) <- rawToChar(readBin(con,raw(),n=1))                                   		  								# 38, char[1]
		.nifti.header.dim_info(headinf) <- rawToChar(readBin(con,raw(),n=1))                                  		  								# 39, char[1], MRI SLICE ORDERING
		.nifti.header.dims(headinf) <- readBin(con,integer(),n=8,size=2,signed=T,endian=.nifti.header.endian(headinf))               # 40, short[8], DATA ARRAY DIMENSIONS
		.nifti.header.intent_p1(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    # 56, float, 1ST INTENT PARAMETER
		.nifti.header.intent_p2(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    # 60, float, 2ND INTENT PARAMETER
		.nifti.header.intent_p3(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                   	# 64, float, 3RD INTENT PARAMETER
		.nifti.header.intent_code(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))        # 68, short, NIFITINTENT CODE
		.nifti.header.datatype(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))           # 70, short, DEFINES DATA TYPE
		.nifti.header.bitpix(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))             # 72, short, NUMBER BITS PER VOXEL
		.nifti.header.slice_start(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))        # 74, short, FIRST SLICE INDEX
		.nifti.header.pixdim(headinf) <- readBin(con,double(),n=8,size=4,endian=.nifti.header.endian(headinf))                       # 76, float[8], GRID SPACINGS
		.nifti.header.vox_offset(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                   #108, float, OFFSET INTO .NII FILE
		.nifti.header.scl_slope(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #112, float, DATA SCALING: SLOPE
		.nifti.header.scl_inter(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #116, float, DATA SCALING: INTERCEPT
		.nifti.header.slice_end (headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))         #120, short, LAST SLICE INDEX
		.nifti.header.slice_code(headinf) <- readBin(con,integer(),size=1,n=1,signed=T,endian=.nifti.header.endian(headinf))         #122, integer[1], SLICE TIMING ORDER
		.nifti.header.xyzt_units(headinf) <- readBin(con,integer(),size=1,n=1,signed=T,endian=.nifti.header.endian(headinf))         #123, integer[1], UNITS OF PIXDIM
		.nifti.header.cal_max(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                      #124, float, MAX DISPLAY INTENSITY
		.nifti.header.cal_min(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                      #128, float, MIN DISPLAY INTENSITY
		.nifti.header.slice_duration(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))               #132, float, TIME FOR 1 SLICE
		.nifti.header.toffset(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                      #136, float, TIME AXIS SHIFT
		.nifti.header.glmax(headinf) <- readBin(con,integer(),n=1,size=4,signed=T,endian=.nifti.header.endian(headinf))              #140, int32
		.nifti.header.glmin(headinf) <- readBin(con,integer(),n=1,size=4,signed=T,endian=.nifti.header.endian(headinf))              #144, int32
		.nifti.header.descrip(headinf) <- rawToChar(readBin(con,raw(),n=80))                                  		  								#148, char[80], TEXT
		.nifti.header.aux_file(headinf) <- rawToChar(readBin(con,raw(),n=24))                                 		  								#228, char[24], AUXILIARY FILENAME
		.nifti.header.qform_code(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))         #252, short, NIFITXFORM CODE
		.nifti.header.sform_code(headinf) <- readBin(con,integer(),n=1,size=2,signed=T,endian=.nifti.header.endian(headinf))         #254, short, NIFITXFORM CODE
		.nifti.header.quatern_b(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #256, float, QUATERNION B PARAM
		.nifti.header.quatern_c(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #260, float, QUATERNION C PARAM
		.nifti.header.quatern_d(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #264, float, QUATERNION D PARAM
		.nifti.header.qoffset_x(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #268, float, QUATERNION X SHIFT
		.nifti.header.qoffset_y(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #272, float, QUATERNION Y SHIFT
		.nifti.header.qoffset_z(headinf) <- readBin(con,double(),n=1,size=4,endian=.nifti.header.endian(headinf))                    #276, float, QUATERNION Z SHIFT
		.nifti.header.srow_x(headinf) <- readBin(con,double(),n=4,size=4,endian=.nifti.header.endian(headinf))                       #280, float[4], 1ST ROW AFFINE TRANSFORM
		.nifti.header.srow_y(headinf) <- readBin(con,double(),n=4,size=4,endian=.nifti.header.endian(headinf))                       #296, float[4], 2ND ROW AFFINE TRANSFORM
		.nifti.header.srow_z(headinf) <- readBin(con,double(),n=4,size=4,endian=.nifti.header.endian(headinf))                       #312, float[4], 3RD ROW AFFINE TRANSFORM
		.nifti.header.intent_name(headinf) <- rawToChar(readBin(con,raw(),n=16))                              		  								#328, char[16], NAME OR MEANING OF DATA
		.nifti.header.magic(headinf) <- rawToChar(readBin(con,raw(),n=4))      	
		
		#close connection
		closeAllConnections()
						
		#set fmri data attributes for read in
		if(.nifti.header.datatype(headinf)==0) { .nifti.header.data.type(headinf)<-'none';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==1) { .nifti.header.data.type(headinf)<-'raw';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==2) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-FALSE } else
		if(.nifti.header.datatype(headinf)==4) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==8) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==16) { .nifti.header.data.type(headinf)<-'double';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==32) { .nifti.header.data.type(headinf)<-'complex';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==64) { .nifti.header.data.type(headinf)<-'double';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==256) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==512) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-FALSE } else
		if(.nifti.header.datatype(headinf)==768) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-FALSE } else
		if(.nifti.header.datatype(headinf)==1024) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==1280) { .nifti.header.data.type(headinf)<-'integer';.nifti.header.data.signed(headinf)<-FALSE } else
		if(.nifti.header.datatype(headinf)==1536) { .nifti.header.data.type(headinf)<-'double';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==1792) { .nifti.header.data.type(headinf)<-'complex';.nifti.header.data.signed(headinf)<-TRUE } else
		if(.nifti.header.datatype(headinf)==2048) { .nifti.header.data.type(headinf)(headinf)<-'complex';.nifti.header.data.signed(headinf)(headinf)<-TRUE } else { stop('Datatype is unknown!') }
		
		
	} else {
		stop('Unable to open connection.\n')
	}

	
	#return nifti.header object
	return(invisible(headinf))

	
}

