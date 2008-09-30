classname <-'settings'
funcname <-'.settings.min.iterlim'
standGen <- function(object) standardGeneric('.settings.min.iterlim')
standMethod <- function(object) object@min.iterlim
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.min.iterlim<-'
standGen <- function(x, value) standardGeneric('.settings.min.iterlim<-')
standMethod <- function(x, value) {x@min.iterlim<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.start.method'
standGen <- function(object) standardGeneric('.settings.start.method')
standMethod <- function(object) object@start.method
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.start.method<-'
standGen <- function(x, value) standardGeneric('.settings.start.method<-')
standMethod <- function(x, value) {x@start.method<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.start.maxfac'
standGen <- function(object) standardGeneric('.settings.start.maxfac')
standMethod <- function(object) object@start.maxfac
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.start.maxfac<-'
standGen <- function(x, value) standardGeneric('.settings.start.maxfac<-')
standMethod <- function(x, value) {x@start.maxfac<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.start.vector'
standGen <- function(object) standardGeneric('.settings.start.vector')
standMethod <- function(object) object@start.vector
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.start.vector<-'
standGen <- function(x, value) standardGeneric('.settings.start.vector<-')
standMethod <- function(x, value) {x@start.vector<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.chk.method'
standGen <- function(object) standardGeneric('.settings.chk.method')
standMethod <- function(object) object@chk.method
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.chk.method<-'
standGen <- function(x, value) standardGeneric('.settings.chk.method<-')
standMethod <- function(x, value) {x@chk.method<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.chk.range'
standGen <- function(object) standardGeneric('.settings.chk.range')
standMethod <- function(object) object@chk.range
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.chk.range<-'
standGen <- function(x, value) standardGeneric('.settings.chk.range<-')
standMethod <- function(x, value) {x@chk.range<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.libname'
standGen <- function(object) standardGeneric('.settings.libname')
standMethod <- function(object) object@libname
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.libname<-'
standGen <- function(x, value) standardGeneric('.settings.libname<-')
standMethod <- function(x, value) {x@libname<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'settings'
funcname <-'.settings.warn'
standGen <- function(object) standardGeneric('.settings.warn')
standMethod <- function(object) object@warn
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.settings.warn<-'
standGen <- function(x, value) standardGeneric('.settings.warn<-')
standMethod <- function(x, value) {x@warn<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.fullpath'
standGen <- function(object) standardGeneric('.nifti.fileinfo.fullpath')
standMethod <- function(object) object@fullpath
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.fullpath<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.fullpath<-')
standMethod <- function(x, value) {x@fullpath<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.filename'
standGen <- function(object) standardGeneric('.nifti.fileinfo.filename')
standMethod <- function(object) object@filename
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.filename<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.filename<-')
standMethod <- function(x, value) {x@filename<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.filetype'
standGen <- function(object) standardGeneric('.nifti.fileinfo.filetype')
standMethod <- function(object) object@filetype
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.filetype<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.filetype<-')
standMethod <- function(x, value) {x@filetype<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.extension'
standGen <- function(object) standardGeneric('.nifti.fileinfo.extension')
standMethod <- function(object) object@extension
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.extension<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.extension<-')
standMethod <- function(x, value) {x@extension<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.gzipped'
standGen <- function(object) standardGeneric('.nifti.fileinfo.gzipped')
standMethod <- function(object) object@gzipped
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.gzipped<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.gzipped<-')
standMethod <- function(x, value) {x@gzipped<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.fileinfo'
funcname <-'.nifti.fileinfo.endian'
standGen <- function(object) standardGeneric('.nifti.fileinfo.endian')
standMethod <- function(object) object@endian
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.fileinfo.endian<-'
standGen <- function(x, value) standardGeneric('.nifti.fileinfo.endian<-')
standMethod <- function(x, value) {x@endian<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.sizeof_hdr'
standGen <- function(object) standardGeneric('.nifti.header.sizeof_hdr')
standMethod <- function(object) object@sizeof_hdr
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.sizeof_hdr<-'
standGen <- function(x, value) standardGeneric('.nifti.header.sizeof_hdr<-')
standMethod <- function(x, value) {x@sizeof_hdr<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.data_type'
standGen <- function(object) standardGeneric('.nifti.header.data_type')
standMethod <- function(object) object@data_type
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.data_type<-'
standGen <- function(x, value) standardGeneric('.nifti.header.data_type<-')
standMethod <- function(x, value) {x@data_type<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.db_name'
standGen <- function(object) standardGeneric('.nifti.header.db_name')
standMethod <- function(object) object@db_name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.db_name<-'
standGen <- function(x, value) standardGeneric('.nifti.header.db_name<-')
standMethod <- function(x, value) {x@db_name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.extents'
standGen <- function(object) standardGeneric('.nifti.header.extents')
standMethod <- function(object) object@extents
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.extents<-'
standGen <- function(x, value) standardGeneric('.nifti.header.extents<-')
standMethod <- function(x, value) {x@extents<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.session_error'
standGen <- function(object) standardGeneric('.nifti.header.session_error')
standMethod <- function(object) object@session_error
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.session_error<-'
standGen <- function(x, value) standardGeneric('.nifti.header.session_error<-')
standMethod <- function(x, value) {x@session_error<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.regular'
standGen <- function(object) standardGeneric('.nifti.header.regular')
standMethod <- function(object) object@regular
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.regular<-'
standGen <- function(x, value) standardGeneric('.nifti.header.regular<-')
standMethod <- function(x, value) {x@regular<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.dim_info'
standGen <- function(object) standardGeneric('.nifti.header.dim_info')
standMethod <- function(object) object@dim_info
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.dim_info<-'
standGen <- function(x, value) standardGeneric('.nifti.header.dim_info<-')
standMethod <- function(x, value) {x@dim_info<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.dims'
standGen <- function(object) standardGeneric('.nifti.header.dims')
standMethod <- function(object) object@dims
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.dims<-'
standGen <- function(x, value) standardGeneric('.nifti.header.dims<-')
standMethod <- function(x, value) {x@dims<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.intent_p1'
standGen <- function(object) standardGeneric('.nifti.header.intent_p1')
standMethod <- function(object) object@intent_p1
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.intent_p1<-'
standGen <- function(x, value) standardGeneric('.nifti.header.intent_p1<-')
standMethod <- function(x, value) {x@intent_p1<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.intent_p2'
standGen <- function(object) standardGeneric('.nifti.header.intent_p2')
standMethod <- function(object) object@intent_p2
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.intent_p2<-'
standGen <- function(x, value) standardGeneric('.nifti.header.intent_p2<-')
standMethod <- function(x, value) {x@intent_p2<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.intent_p3'
standGen <- function(object) standardGeneric('.nifti.header.intent_p3')
standMethod <- function(object) object@intent_p3
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.intent_p3<-'
standGen <- function(x, value) standardGeneric('.nifti.header.intent_p3<-')
standMethod <- function(x, value) {x@intent_p3<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.intent_code'
standGen <- function(object) standardGeneric('.nifti.header.intent_code')
standMethod <- function(object) object@intent_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.intent_code<-'
standGen <- function(x, value) standardGeneric('.nifti.header.intent_code<-')
standMethod <- function(x, value) {x@intent_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.datatype'
standGen <- function(object) standardGeneric('.nifti.header.datatype')
standMethod <- function(object) object@datatype
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.datatype<-'
standGen <- function(x, value) standardGeneric('.nifti.header.datatype<-')
standMethod <- function(x, value) {x@datatype<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.bitpix'
standGen <- function(object) standardGeneric('.nifti.header.bitpix')
standMethod <- function(object) object@bitpix
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.bitpix<-'
standGen <- function(x, value) standardGeneric('.nifti.header.bitpix<-')
standMethod <- function(x, value) {x@bitpix<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.slice_start'
standGen <- function(object) standardGeneric('.nifti.header.slice_start')
standMethod <- function(object) object@slice_start
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.slice_start<-'
standGen <- function(x, value) standardGeneric('.nifti.header.slice_start<-')
standMethod <- function(x, value) {x@slice_start<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.pixdim'
standGen <- function(object) standardGeneric('.nifti.header.pixdim')
standMethod <- function(object) object@pixdim
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.pixdim<-'
standGen <- function(x, value) standardGeneric('.nifti.header.pixdim<-')
standMethod <- function(x, value) {x@pixdim<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.vox_offset'
standGen <- function(object) standardGeneric('.nifti.header.vox_offset')
standMethod <- function(object) object@vox_offset
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.vox_offset<-'
standGen <- function(x, value) standardGeneric('.nifti.header.vox_offset<-')
standMethod <- function(x, value) {x@vox_offset<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.scl_slope'
standGen <- function(object) standardGeneric('.nifti.header.scl_slope')
standMethod <- function(object) object@scl_slope
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.scl_slope<-'
standGen <- function(x, value) standardGeneric('.nifti.header.scl_slope<-')
standMethod <- function(x, value) {x@scl_slope<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.scl_inter'
standGen <- function(object) standardGeneric('.nifti.header.scl_inter')
standMethod <- function(object) object@scl_inter
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.scl_inter<-'
standGen <- function(x, value) standardGeneric('.nifti.header.scl_inter<-')
standMethod <- function(x, value) {x@scl_inter<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.slice_end'
standGen <- function(object) standardGeneric('.nifti.header.slice_end')
standMethod <- function(object) object@slice_end
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.slice_end<-'
standGen <- function(x, value) standardGeneric('.nifti.header.slice_end<-')
standMethod <- function(x, value) {x@slice_end<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.slice_code'
standGen <- function(object) standardGeneric('.nifti.header.slice_code')
standMethod <- function(object) object@slice_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.slice_code<-'
standGen <- function(x, value) standardGeneric('.nifti.header.slice_code<-')
standMethod <- function(x, value) {x@slice_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.xyzt_units'
standGen <- function(object) standardGeneric('.nifti.header.xyzt_units')
standMethod <- function(object) object@xyzt_units
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.xyzt_units<-'
standGen <- function(x, value) standardGeneric('.nifti.header.xyzt_units<-')
standMethod <- function(x, value) {x@xyzt_units<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.cal_max'
standGen <- function(object) standardGeneric('.nifti.header.cal_max')
standMethod <- function(object) object@cal_max
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.cal_max<-'
standGen <- function(x, value) standardGeneric('.nifti.header.cal_max<-')
standMethod <- function(x, value) {x@cal_max<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.cal_min'
standGen <- function(object) standardGeneric('.nifti.header.cal_min')
standMethod <- function(object) object@cal_min
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.cal_min<-'
standGen <- function(x, value) standardGeneric('.nifti.header.cal_min<-')
standMethod <- function(x, value) {x@cal_min<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.slice_duration'
standGen <- function(object) standardGeneric('.nifti.header.slice_duration')
standMethod <- function(object) object@slice_duration
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.slice_duration<-'
standGen <- function(x, value) standardGeneric('.nifti.header.slice_duration<-')
standMethod <- function(x, value) {x@slice_duration<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.toffset'
standGen <- function(object) standardGeneric('.nifti.header.toffset')
standMethod <- function(object) object@toffset
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.toffset<-'
standGen <- function(x, value) standardGeneric('.nifti.header.toffset<-')
standMethod <- function(x, value) {x@toffset<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.glmax'
standGen <- function(object) standardGeneric('.nifti.header.glmax')
standMethod <- function(object) object@glmax
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.glmax<-'
standGen <- function(x, value) standardGeneric('.nifti.header.glmax<-')
standMethod <- function(x, value) {x@glmax<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.glmin'
standGen <- function(object) standardGeneric('.nifti.header.glmin')
standMethod <- function(object) object@glmin
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.glmin<-'
standGen <- function(x, value) standardGeneric('.nifti.header.glmin<-')
standMethod <- function(x, value) {x@glmin<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.descrip'
standGen <- function(object) standardGeneric('.nifti.header.descrip')
standMethod <- function(object) object@descrip
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.descrip<-'
standGen <- function(x, value) standardGeneric('.nifti.header.descrip<-')
standMethod <- function(x, value) {x@descrip<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.aux_file'
standGen <- function(object) standardGeneric('.nifti.header.aux_file')
standMethod <- function(object) object@aux_file
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.aux_file<-'
standGen <- function(x, value) standardGeneric('.nifti.header.aux_file<-')
standMethod <- function(x, value) {x@aux_file<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.qform_code'
standGen <- function(object) standardGeneric('.nifti.header.qform_code')
standMethod <- function(object) object@qform_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.qform_code<-'
standGen <- function(x, value) standardGeneric('.nifti.header.qform_code<-')
standMethod <- function(x, value) {x@qform_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.sform_code'
standGen <- function(object) standardGeneric('.nifti.header.sform_code')
standMethod <- function(object) object@sform_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.sform_code<-'
standGen <- function(x, value) standardGeneric('.nifti.header.sform_code<-')
standMethod <- function(x, value) {x@sform_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.quatern_b'
standGen <- function(object) standardGeneric('.nifti.header.quatern_b')
standMethod <- function(object) object@quatern_b
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.quatern_b<-'
standGen <- function(x, value) standardGeneric('.nifti.header.quatern_b<-')
standMethod <- function(x, value) {x@quatern_b<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.quatern_c'
standGen <- function(object) standardGeneric('.nifti.header.quatern_c')
standMethod <- function(object) object@quatern_c
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.quatern_c<-'
standGen <- function(x, value) standardGeneric('.nifti.header.quatern_c<-')
standMethod <- function(x, value) {x@quatern_c<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.quatern_d'
standGen <- function(object) standardGeneric('.nifti.header.quatern_d')
standMethod <- function(object) object@quatern_d
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.quatern_d<-'
standGen <- function(x, value) standardGeneric('.nifti.header.quatern_d<-')
standMethod <- function(x, value) {x@quatern_d<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.qoffset_x'
standGen <- function(object) standardGeneric('.nifti.header.qoffset_x')
standMethod <- function(object) object@qoffset_x
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.qoffset_x<-'
standGen <- function(x, value) standardGeneric('.nifti.header.qoffset_x<-')
standMethod <- function(x, value) {x@qoffset_x<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.qoffset_y'
standGen <- function(object) standardGeneric('.nifti.header.qoffset_y')
standMethod <- function(object) object@qoffset_y
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.qoffset_y<-'
standGen <- function(x, value) standardGeneric('.nifti.header.qoffset_y<-')
standMethod <- function(x, value) {x@qoffset_y<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.qoffset_z'
standGen <- function(object) standardGeneric('.nifti.header.qoffset_z')
standMethod <- function(object) object@qoffset_z
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.qoffset_z<-'
standGen <- function(x, value) standardGeneric('.nifti.header.qoffset_z<-')
standMethod <- function(x, value) {x@qoffset_z<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.srow_x'
standGen <- function(object) standardGeneric('.nifti.header.srow_x')
standMethod <- function(object) object@srow_x
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.srow_x<-'
standGen <- function(x, value) standardGeneric('.nifti.header.srow_x<-')
standMethod <- function(x, value) {x@srow_x<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.srow_y'
standGen <- function(object) standardGeneric('.nifti.header.srow_y')
standMethod <- function(object) object@srow_y
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.srow_y<-'
standGen <- function(x, value) standardGeneric('.nifti.header.srow_y<-')
standMethod <- function(x, value) {x@srow_y<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.srow_z'
standGen <- function(object) standardGeneric('.nifti.header.srow_z')
standMethod <- function(object) object@srow_z
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.srow_z<-'
standGen <- function(x, value) standardGeneric('.nifti.header.srow_z<-')
standMethod <- function(x, value) {x@srow_z<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.intent_name'
standGen <- function(object) standardGeneric('.nifti.header.intent_name')
standMethod <- function(object) object@intent_name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.intent_name<-'
standGen <- function(x, value) standardGeneric('.nifti.header.intent_name<-')
standMethod <- function(x, value) {x@intent_name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.magic'
standGen <- function(object) standardGeneric('.nifti.header.magic')
standMethod <- function(object) object@magic
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.magic<-'
standGen <- function(x, value) standardGeneric('.nifti.header.magic<-')
standMethod <- function(x, value) {x@magic<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.data.type'
standGen <- function(object) standardGeneric('.nifti.header.data.type')
standMethod <- function(object) object@data.type
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.data.type<-'
standGen <- function(x, value) standardGeneric('.nifti.header.data.type<-')
standMethod <- function(x, value) {x@data.type<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.data.signed'
standGen <- function(object) standardGeneric('.nifti.header.data.signed')
standMethod <- function(object) object@data.signed
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.data.signed<-'
standGen <- function(x, value) standardGeneric('.nifti.header.data.signed<-')
standMethod <- function(x, value) {x@data.signed<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.fullpath'
standGen <- function(object) standardGeneric('.nifti.header.fullpath')
standMethod <- function(object) object@fullpath
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.fullpath<-'
standGen <- function(x, value) standardGeneric('.nifti.header.fullpath<-')
standMethod <- function(x, value) {x@fullpath<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.filename'
standGen <- function(object) standardGeneric('.nifti.header.filename')
standMethod <- function(object) object@filename
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.filename<-'
standGen <- function(x, value) standardGeneric('.nifti.header.filename<-')
standMethod <- function(x, value) {x@filename<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.filetype'
standGen <- function(object) standardGeneric('.nifti.header.filetype')
standMethod <- function(object) object@filetype
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.filetype<-'
standGen <- function(x, value) standardGeneric('.nifti.header.filetype<-')
standMethod <- function(x, value) {x@filetype<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.extension'
standGen <- function(object) standardGeneric('.nifti.header.extension')
standMethod <- function(object) object@extension
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.extension<-'
standGen <- function(x, value) standardGeneric('.nifti.header.extension<-')
standMethod <- function(x, value) {x@extension<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.gzipped'
standGen <- function(object) standardGeneric('.nifti.header.gzipped')
standMethod <- function(object) object@gzipped
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.gzipped<-'
standGen <- function(x, value) standardGeneric('.nifti.header.gzipped<-')
standMethod <- function(x, value) {x@gzipped<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'nifti.header'
funcname <-'.nifti.header.endian'
standGen <- function(object) standardGeneric('.nifti.header.endian')
standMethod <- function(object) object@endian
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.nifti.header.endian<-'
standGen <- function(x, value) standardGeneric('.nifti.header.endian<-')
standMethod <- function(x, value) {x@endian<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.datavec'
standGen <- function(object) standardGeneric('.fmri.data.datavec')
standMethod <- function(object) object@datavec
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.datavec<-'
standGen <- function(x, value) standardGeneric('.fmri.data.datavec<-')
standMethod <- function(x, value) {x@datavec<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.sizeof_hdr'
standGen <- function(object) standardGeneric('.fmri.data.sizeof_hdr')
standMethod <- function(object) object@sizeof_hdr
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.sizeof_hdr<-'
standGen <- function(x, value) standardGeneric('.fmri.data.sizeof_hdr<-')
standMethod <- function(x, value) {x@sizeof_hdr<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.data_type'
standGen <- function(object) standardGeneric('.fmri.data.data_type')
standMethod <- function(object) object@data_type
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.data_type<-'
standGen <- function(x, value) standardGeneric('.fmri.data.data_type<-')
standMethod <- function(x, value) {x@data_type<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.db_name'
standGen <- function(object) standardGeneric('.fmri.data.db_name')
standMethod <- function(object) object@db_name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.db_name<-'
standGen <- function(x, value) standardGeneric('.fmri.data.db_name<-')
standMethod <- function(x, value) {x@db_name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.extents'
standGen <- function(object) standardGeneric('.fmri.data.extents')
standMethod <- function(object) object@extents
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.extents<-'
standGen <- function(x, value) standardGeneric('.fmri.data.extents<-')
standMethod <- function(x, value) {x@extents<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.session_error'
standGen <- function(object) standardGeneric('.fmri.data.session_error')
standMethod <- function(object) object@session_error
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.session_error<-'
standGen <- function(x, value) standardGeneric('.fmri.data.session_error<-')
standMethod <- function(x, value) {x@session_error<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.regular'
standGen <- function(object) standardGeneric('.fmri.data.regular')
standMethod <- function(object) object@regular
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.regular<-'
standGen <- function(x, value) standardGeneric('.fmri.data.regular<-')
standMethod <- function(x, value) {x@regular<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.dim_info'
standGen <- function(object) standardGeneric('.fmri.data.dim_info')
standMethod <- function(object) object@dim_info
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.dim_info<-'
standGen <- function(x, value) standardGeneric('.fmri.data.dim_info<-')
standMethod <- function(x, value) {x@dim_info<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.dims'
standGen <- function(object) standardGeneric('.fmri.data.dims')
standMethod <- function(object) object@dims
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.dims<-'
standGen <- function(x, value) standardGeneric('.fmri.data.dims<-')
standMethod <- function(x, value) {x@dims<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.intent_p1'
standGen <- function(object) standardGeneric('.fmri.data.intent_p1')
standMethod <- function(object) object@intent_p1
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.intent_p1<-'
standGen <- function(x, value) standardGeneric('.fmri.data.intent_p1<-')
standMethod <- function(x, value) {x@intent_p1<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.intent_p2'
standGen <- function(object) standardGeneric('.fmri.data.intent_p2')
standMethod <- function(object) object@intent_p2
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.intent_p2<-'
standGen <- function(x, value) standardGeneric('.fmri.data.intent_p2<-')
standMethod <- function(x, value) {x@intent_p2<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.intent_p3'
standGen <- function(object) standardGeneric('.fmri.data.intent_p3')
standMethod <- function(object) object@intent_p3
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.intent_p3<-'
standGen <- function(x, value) standardGeneric('.fmri.data.intent_p3<-')
standMethod <- function(x, value) {x@intent_p3<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.intent_code'
standGen <- function(object) standardGeneric('.fmri.data.intent_code')
standMethod <- function(object) object@intent_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.intent_code<-'
standGen <- function(x, value) standardGeneric('.fmri.data.intent_code<-')
standMethod <- function(x, value) {x@intent_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.datatype'
standGen <- function(object) standardGeneric('.fmri.data.datatype')
standMethod <- function(object) object@datatype
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.datatype<-'
standGen <- function(x, value) standardGeneric('.fmri.data.datatype<-')
standMethod <- function(x, value) {x@datatype<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.bitpix'
standGen <- function(object) standardGeneric('.fmri.data.bitpix')
standMethod <- function(object) object@bitpix
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.bitpix<-'
standGen <- function(x, value) standardGeneric('.fmri.data.bitpix<-')
standMethod <- function(x, value) {x@bitpix<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.slice_start'
standGen <- function(object) standardGeneric('.fmri.data.slice_start')
standMethod <- function(object) object@slice_start
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.slice_start<-'
standGen <- function(x, value) standardGeneric('.fmri.data.slice_start<-')
standMethod <- function(x, value) {x@slice_start<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.pixdim'
standGen <- function(object) standardGeneric('.fmri.data.pixdim')
standMethod <- function(object) object@pixdim
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.pixdim<-'
standGen <- function(x, value) standardGeneric('.fmri.data.pixdim<-')
standMethod <- function(x, value) {x@pixdim<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.vox_offset'
standGen <- function(object) standardGeneric('.fmri.data.vox_offset')
standMethod <- function(object) object@vox_offset
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.vox_offset<-'
standGen <- function(x, value) standardGeneric('.fmri.data.vox_offset<-')
standMethod <- function(x, value) {x@vox_offset<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.scl_slope'
standGen <- function(object) standardGeneric('.fmri.data.scl_slope')
standMethod <- function(object) object@scl_slope
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.scl_slope<-'
standGen <- function(x, value) standardGeneric('.fmri.data.scl_slope<-')
standMethod <- function(x, value) {x@scl_slope<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.scl_inter'
standGen <- function(object) standardGeneric('.fmri.data.scl_inter')
standMethod <- function(object) object@scl_inter
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.scl_inter<-'
standGen <- function(x, value) standardGeneric('.fmri.data.scl_inter<-')
standMethod <- function(x, value) {x@scl_inter<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.slice_end'
standGen <- function(object) standardGeneric('.fmri.data.slice_end')
standMethod <- function(object) object@slice_end
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.slice_end<-'
standGen <- function(x, value) standardGeneric('.fmri.data.slice_end<-')
standMethod <- function(x, value) {x@slice_end<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.slice_code'
standGen <- function(object) standardGeneric('.fmri.data.slice_code')
standMethod <- function(object) object@slice_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.slice_code<-'
standGen <- function(x, value) standardGeneric('.fmri.data.slice_code<-')
standMethod <- function(x, value) {x@slice_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.xyzt_units'
standGen <- function(object) standardGeneric('.fmri.data.xyzt_units')
standMethod <- function(object) object@xyzt_units
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.xyzt_units<-'
standGen <- function(x, value) standardGeneric('.fmri.data.xyzt_units<-')
standMethod <- function(x, value) {x@xyzt_units<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.cal_max'
standGen <- function(object) standardGeneric('.fmri.data.cal_max')
standMethod <- function(object) object@cal_max
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.cal_max<-'
standGen <- function(x, value) standardGeneric('.fmri.data.cal_max<-')
standMethod <- function(x, value) {x@cal_max<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.cal_min'
standGen <- function(object) standardGeneric('.fmri.data.cal_min')
standMethod <- function(object) object@cal_min
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.cal_min<-'
standGen <- function(x, value) standardGeneric('.fmri.data.cal_min<-')
standMethod <- function(x, value) {x@cal_min<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.slice_duration'
standGen <- function(object) standardGeneric('.fmri.data.slice_duration')
standMethod <- function(object) object@slice_duration
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.slice_duration<-'
standGen <- function(x, value) standardGeneric('.fmri.data.slice_duration<-')
standMethod <- function(x, value) {x@slice_duration<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.toffset'
standGen <- function(object) standardGeneric('.fmri.data.toffset')
standMethod <- function(object) object@toffset
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.toffset<-'
standGen <- function(x, value) standardGeneric('.fmri.data.toffset<-')
standMethod <- function(x, value) {x@toffset<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.glmax'
standGen <- function(object) standardGeneric('.fmri.data.glmax')
standMethod <- function(object) object@glmax
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.glmax<-'
standGen <- function(x, value) standardGeneric('.fmri.data.glmax<-')
standMethod <- function(x, value) {x@glmax<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.glmin'
standGen <- function(object) standardGeneric('.fmri.data.glmin')
standMethod <- function(object) object@glmin
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.glmin<-'
standGen <- function(x, value) standardGeneric('.fmri.data.glmin<-')
standMethod <- function(x, value) {x@glmin<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.descrip'
standGen <- function(object) standardGeneric('.fmri.data.descrip')
standMethod <- function(object) object@descrip
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.descrip<-'
standGen <- function(x, value) standardGeneric('.fmri.data.descrip<-')
standMethod <- function(x, value) {x@descrip<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.aux_file'
standGen <- function(object) standardGeneric('.fmri.data.aux_file')
standMethod <- function(object) object@aux_file
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.aux_file<-'
standGen <- function(x, value) standardGeneric('.fmri.data.aux_file<-')
standMethod <- function(x, value) {x@aux_file<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.qform_code'
standGen <- function(object) standardGeneric('.fmri.data.qform_code')
standMethod <- function(object) object@qform_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.qform_code<-'
standGen <- function(x, value) standardGeneric('.fmri.data.qform_code<-')
standMethod <- function(x, value) {x@qform_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.sform_code'
standGen <- function(object) standardGeneric('.fmri.data.sform_code')
standMethod <- function(object) object@sform_code
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.sform_code<-'
standGen <- function(x, value) standardGeneric('.fmri.data.sform_code<-')
standMethod <- function(x, value) {x@sform_code<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.quatern_b'
standGen <- function(object) standardGeneric('.fmri.data.quatern_b')
standMethod <- function(object) object@quatern_b
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.quatern_b<-'
standGen <- function(x, value) standardGeneric('.fmri.data.quatern_b<-')
standMethod <- function(x, value) {x@quatern_b<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.quatern_c'
standGen <- function(object) standardGeneric('.fmri.data.quatern_c')
standMethod <- function(object) object@quatern_c
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.quatern_c<-'
standGen <- function(x, value) standardGeneric('.fmri.data.quatern_c<-')
standMethod <- function(x, value) {x@quatern_c<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.quatern_d'
standGen <- function(object) standardGeneric('.fmri.data.quatern_d')
standMethod <- function(object) object@quatern_d
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.quatern_d<-'
standGen <- function(x, value) standardGeneric('.fmri.data.quatern_d<-')
standMethod <- function(x, value) {x@quatern_d<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.qoffset_x'
standGen <- function(object) standardGeneric('.fmri.data.qoffset_x')
standMethod <- function(object) object@qoffset_x
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.qoffset_x<-'
standGen <- function(x, value) standardGeneric('.fmri.data.qoffset_x<-')
standMethod <- function(x, value) {x@qoffset_x<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.qoffset_y'
standGen <- function(object) standardGeneric('.fmri.data.qoffset_y')
standMethod <- function(object) object@qoffset_y
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.qoffset_y<-'
standGen <- function(x, value) standardGeneric('.fmri.data.qoffset_y<-')
standMethod <- function(x, value) {x@qoffset_y<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.qoffset_z'
standGen <- function(object) standardGeneric('.fmri.data.qoffset_z')
standMethod <- function(object) object@qoffset_z
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.qoffset_z<-'
standGen <- function(x, value) standardGeneric('.fmri.data.qoffset_z<-')
standMethod <- function(x, value) {x@qoffset_z<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.srow_x'
standGen <- function(object) standardGeneric('.fmri.data.srow_x')
standMethod <- function(object) object@srow_x
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.srow_x<-'
standGen <- function(x, value) standardGeneric('.fmri.data.srow_x<-')
standMethod <- function(x, value) {x@srow_x<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.srow_y'
standGen <- function(object) standardGeneric('.fmri.data.srow_y')
standMethod <- function(object) object@srow_y
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.srow_y<-'
standGen <- function(x, value) standardGeneric('.fmri.data.srow_y<-')
standMethod <- function(x, value) {x@srow_y<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.srow_z'
standGen <- function(object) standardGeneric('.fmri.data.srow_z')
standMethod <- function(object) object@srow_z
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.srow_z<-'
standGen <- function(x, value) standardGeneric('.fmri.data.srow_z<-')
standMethod <- function(x, value) {x@srow_z<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.intent_name'
standGen <- function(object) standardGeneric('.fmri.data.intent_name')
standMethod <- function(object) object@intent_name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.intent_name<-'
standGen <- function(x, value) standardGeneric('.fmri.data.intent_name<-')
standMethod <- function(x, value) {x@intent_name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.magic'
standGen <- function(object) standardGeneric('.fmri.data.magic')
standMethod <- function(object) object@magic
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.magic<-'
standGen <- function(x, value) standardGeneric('.fmri.data.magic<-')
standMethod <- function(x, value) {x@magic<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.data.type'
standGen <- function(object) standardGeneric('.fmri.data.data.type')
standMethod <- function(object) object@data.type
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.data.type<-'
standGen <- function(x, value) standardGeneric('.fmri.data.data.type<-')
standMethod <- function(x, value) {x@data.type<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.data.signed'
standGen <- function(object) standardGeneric('.fmri.data.data.signed')
standMethod <- function(object) object@data.signed
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.data.signed<-'
standGen <- function(x, value) standardGeneric('.fmri.data.data.signed<-')
standMethod <- function(x, value) {x@data.signed<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.fullpath'
standGen <- function(object) standardGeneric('.fmri.data.fullpath')
standMethod <- function(object) object@fullpath
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.fullpath<-'
standGen <- function(x, value) standardGeneric('.fmri.data.fullpath<-')
standMethod <- function(x, value) {x@fullpath<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.filename'
standGen <- function(object) standardGeneric('.fmri.data.filename')
standMethod <- function(object) object@filename
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.filename<-'
standGen <- function(x, value) standardGeneric('.fmri.data.filename<-')
standMethod <- function(x, value) {x@filename<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.filetype'
standGen <- function(object) standardGeneric('.fmri.data.filetype')
standMethod <- function(object) object@filetype
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.filetype<-'
standGen <- function(x, value) standardGeneric('.fmri.data.filetype<-')
standMethod <- function(x, value) {x@filetype<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.extension'
standGen <- function(object) standardGeneric('.fmri.data.extension')
standMethod <- function(object) object@extension
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.extension<-'
standGen <- function(x, value) standardGeneric('.fmri.data.extension<-')
standMethod <- function(x, value) {x@extension<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.gzipped'
standGen <- function(object) standardGeneric('.fmri.data.gzipped')
standMethod <- function(object) object@gzipped
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.gzipped<-'
standGen <- function(x, value) standardGeneric('.fmri.data.gzipped<-')
standMethod <- function(x, value) {x@gzipped<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'fmri.data'
funcname <-'.fmri.data.endian'
standGen <- function(object) standardGeneric('.fmri.data.endian')
standMethod <- function(object) object@endian
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.fmri.data.endian<-'
standGen <- function(x, value) standardGeneric('.fmri.data.endian<-')
standMethod <- function(x, value) {x@endian<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.current'
standGen <- function(object) standardGeneric('.sequence.current')
standMethod <- function(object) object@current
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.current<-'
standGen <- function(x, value) standardGeneric('.sequence.current<-')
standMethod <- function(x, value) {x@current<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.regions'
standGen <- function(object) standardGeneric('.sequence.regions')
standMethod <- function(object) object@regions
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.regions<-'
standGen <- function(x, value) standardGeneric('.sequence.regions<-')
standMethod <- function(x, value) {x@regions<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.mnames'
standGen <- function(object) standardGeneric('.sequence.mnames')
standMethod <- function(object) object@mnames
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.mnames<-'
standGen <- function(x, value) standardGeneric('.sequence.mnames<-')
standMethod <- function(x, value) {x@mnames<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.fit'
standGen <- function(object) standardGeneric('.sequence.fit')
standMethod <- function(object) object@fit
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.fit<-'
standGen <- function(x, value) standardGeneric('.sequence.fit<-')
standMethod <- function(x, value) {x@fit<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.minimum'
standGen <- function(object) standardGeneric('.sequence.minimum')
standMethod <- function(object) object@minimum
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.minimum<-'
standGen <- function(x, value) standardGeneric('.sequence.minimum<-')
standMethod <- function(x, value) {x@minimum<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sequence'
funcname <-'.sequence.valid'
standGen <- function(object) standardGeneric('.sequence.valid')
standMethod <- function(object) object@valid
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sequence.valid<-'
standGen <- function(x, value) standardGeneric('.sequence.valid<-')
standMethod <- function(x, value) {x@valid<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'wald'
funcname <-'.wald.design'
standGen <- function(object) standardGeneric('.wald.design')
standMethod <- function(object) object@design
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.wald.design<-'
standGen <- function(x, value) standardGeneric('.wald.design<-')
standMethod <- function(x, value) {x@design<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'wald'
funcname <-'.wald.stats'
standGen <- function(object) standardGeneric('.wald.stats')
standMethod <- function(object) object@stats
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.wald.stats<-'
standGen <- function(x, value) standardGeneric('.wald.stats<-')
standMethod <- function(x, value) {x@stats<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'wald'
funcname <-'.wald.df1'
standGen <- function(object) standardGeneric('.wald.df1')
standMethod <- function(object) object@df1
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.wald.df1<-'
standGen <- function(x, value) standardGeneric('.wald.df1<-')
standMethod <- function(x, value) {x@df1<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'wald'
funcname <-'.wald.df2'
standGen <- function(object) standardGeneric('.wald.df2')
standMethod <- function(object) object@df2
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.wald.df2<-'
standGen <- function(x, value) standardGeneric('.wald.df2<-')
standMethod <- function(x, value) {x@df2<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'wald'
funcname <-'.wald.pvalues'
standGen <- function(object) standardGeneric('.wald.pvalues')
standMethod <- function(object) object@pvalues
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.wald.pvalues<-'
standGen <- function(x, value) standardGeneric('.wald.pvalues<-')
standMethod <- function(x, value) {x@pvalues<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'analysis'
funcname <-'.analysis.name'
standGen <- function(object) standardGeneric('.analysis.name')
standMethod <- function(object) object@name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.analysis.name<-'
standGen <- function(x, value) standardGeneric('.analysis.name<-')
standMethod <- function(x, value) {x@name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'analysis'
funcname <-'.analysis.modelobjects'
standGen <- function(object) standardGeneric('.analysis.modelobjects')
standMethod <- function(object) object@modelobjects
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.analysis.modelobjects<-'
standGen <- function(x, value) standardGeneric('.analysis.modelobjects<-')
standMethod <- function(x, value) {x@modelobjects<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'analysis'
funcname <-'.analysis.designmatrix'
standGen <- function(object) standardGeneric('.analysis.designmatrix')
standMethod <- function(object) object@designmatrix
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.analysis.designmatrix<-'
standGen <- function(x, value) standardGeneric('.analysis.designmatrix<-')
standMethod <- function(x, value) {x@designmatrix<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'analysis'
funcname <-'.analysis.rfx'
standGen <- function(object) standardGeneric('.analysis.rfx')
standMethod <- function(object) object@rfx
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.analysis.rfx<-'
standGen <- function(x, value) standardGeneric('.analysis.rfx<-')
standMethod <- function(x, value) {x@rfx<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.numsims'
standGen <- function(object) standardGeneric('.sims.numsims')
standMethod <- function(object) object@numsims
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.numsims<-'
standGen <- function(x, value) standardGeneric('.sims.numsims<-')
standMethod <- function(x, value) {x@numsims<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.numtrials'
standGen <- function(object) standardGeneric('.sims.numtrials')
standMethod <- function(object) object@numtrials
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.numtrials<-'
standGen <- function(x, value) standardGeneric('.sims.numtrials<-')
standMethod <- function(x, value) {x@numtrials<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.theta'
standGen <- function(object) standardGeneric('.sims.theta')
standMethod <- function(object) object@theta
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.theta<-'
standGen <- function(x, value) standardGeneric('.sims.theta<-')
standMethod <- function(x, value) {x@theta<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.shapemodel'
standGen <- function(object) standardGeneric('.sims.shapemodel')
standMethod <- function(object) object@shapemodel
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.shapemodel<-'
standGen <- function(x, value) standardGeneric('.sims.shapemodel<-')
standMethod <- function(x, value) {x@shapemodel<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.noisemodel'
standGen <- function(object) standardGeneric('.sims.noisemodel')
standMethod <- function(object) object@noisemodel
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.noisemodel<-'
standGen <- function(x, value) standardGeneric('.sims.noisemodel<-')
standMethod <- function(x, value) {x@noisemodel<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.noiseFWHM'
standGen <- function(object) standardGeneric('.sims.noiseFWHM')
standMethod <- function(object) object@noiseFWHM
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.noiseFWHM<-'
standGen <- function(x, value) standardGeneric('.sims.noiseFWHM<-')
standMethod <- function(x, value) {x@noiseFWHM<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.imageFWHM'
standGen <- function(object) standardGeneric('.sims.imageFWHM')
standMethod <- function(object) object@imageFWHM
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.imageFWHM<-'
standGen <- function(x, value) standardGeneric('.sims.imageFWHM<-')
standMethod <- function(x, value) {x@imageFWHM<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.sequence'
standGen <- function(object) standardGeneric('.sims.sequence')
standMethod <- function(object) object@sequence
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.sequence<-'
standGen <- function(x, value) standardGeneric('.sims.sequence<-')
standMethod <- function(x, value) {x@sequence<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'sims'
funcname <-'.sims.SNR'
standGen <- function(object) standardGeneric('.sims.SNR')
standMethod <- function(object) object@SNR
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.sims.SNR<-'
standGen <- function(x, value) standardGeneric('.sims.SNR<-')
standMethod <- function(x, value) {x@SNR<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.modelname'
standGen <- function(object) standardGeneric('.model.modelname')
standMethod <- function(object) object@modelname
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.modelname<-'
standGen <- function(x, value) standardGeneric('.model.modelname<-')
standMethod <- function(x, value) {x@modelname<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.convergence'
standGen <- function(object) standardGeneric('.model.convergence')
standMethod <- function(object) object@convergence
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.convergence<-'
standGen <- function(x, value) standardGeneric('.model.convergence<-')
standMethod <- function(x, value) {x@convergence<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.minimum'
standGen <- function(object) standardGeneric('.model.minimum')
standMethod <- function(object) object@minimum
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.minimum<-'
standGen <- function(x, value) standardGeneric('.model.minimum<-')
standMethod <- function(x, value) {x@minimum<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.estimates'
standGen <- function(object) standardGeneric('.model.estimates')
standMethod <- function(object) object@estimates
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.estimates<-'
standGen <- function(x, value) standardGeneric('.model.estimates<-')
standMethod <- function(x, value) {x@estimates<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.hessian'
standGen <- function(object) standardGeneric('.model.hessian')
standMethod <- function(object) object@hessian
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.hessian<-'
standGen <- function(x, value) standardGeneric('.model.hessian<-')
standMethod <- function(x, value) {x@hessian<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.varcov'
standGen <- function(object) standardGeneric('.model.varcov')
standMethod <- function(object) object@varcov
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.varcov<-'
standGen <- function(x, value) standardGeneric('.model.varcov<-')
standMethod <- function(x, value) {x@varcov<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.warnings'
standGen <- function(object) standardGeneric('.model.warnings')
standMethod <- function(object) object@warnings
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.warnings<-'
standGen <- function(x, value) standardGeneric('.model.warnings<-')
standMethod <- function(x, value) {x@warnings<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.fit'
standGen <- function(object) standardGeneric('.model.fit')
standMethod <- function(object) object@fit
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.fit<-'
standGen <- function(x, value) standardGeneric('.model.fit<-')
standMethod <- function(x, value) {x@fit<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.wald'
standGen <- function(object) standardGeneric('.model.wald')
standMethod <- function(object) object@wald
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.wald<-'
standGen <- function(x, value) standardGeneric('.model.wald<-')
standMethod <- function(x, value) {x@wald<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.regions'
standGen <- function(object) standardGeneric('.model.regions')
standMethod <- function(object) object@regions
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.regions<-'
standGen <- function(x, value) standardGeneric('.model.regions<-')
standMethod <- function(x, value) {x@regions<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.startval'
standGen <- function(object) standardGeneric('.model.startval')
standMethod <- function(object) object@startval
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.startval<-'
standGen <- function(x, value) standardGeneric('.model.startval<-')
standMethod <- function(x, value) {x@startval<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.proctime'
standGen <- function(object) standardGeneric('.model.proctime')
standMethod <- function(object) object@proctime
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.proctime<-'
standGen <- function(x, value) standardGeneric('.model.proctime<-')
standMethod <- function(x, value) {x@proctime<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.valid'
standGen <- function(object) standardGeneric('.model.valid')
standMethod <- function(object) object@valid
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.valid<-'
standGen <- function(x, value) standardGeneric('.model.valid<-')
standMethod <- function(x, value) {x@valid<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.name'
standGen <- function(object) standardGeneric('.model.name')
standMethod <- function(object) object@name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.name<-'
standGen <- function(x, value) standardGeneric('.model.name<-')
standMethod <- function(x, value) {x@name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.fullpath'
standGen <- function(object) standardGeneric('.model.fullpath')
standMethod <- function(object) object@fullpath
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.fullpath<-'
standGen <- function(x, value) standardGeneric('.model.fullpath<-')
standMethod <- function(x, value) {x@fullpath<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.datafiles'
standGen <- function(object) standardGeneric('.model.datafiles')
standMethod <- function(object) object@datafiles
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.datafiles<-'
standGen <- function(x, value) standardGeneric('.model.datafiles<-')
standMethod <- function(x, value) {x@datafiles<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.weightfiles'
standGen <- function(object) standardGeneric('.model.weightfiles')
standMethod <- function(object) object@weightfiles
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.weightfiles<-'
standGen <- function(x, value) standardGeneric('.model.weightfiles<-')
standMethod <- function(x, value) {x@weightfiles<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.avgdatfile'
standGen <- function(object) standardGeneric('.model.avgdatfile')
standMethod <- function(object) object@avgdatfile
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.avgdatfile<-'
standGen <- function(x, value) standardGeneric('.model.avgdatfile<-')
standMethod <- function(x, value) {x@avgdatfile<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.avgWfile'
standGen <- function(object) standardGeneric('.model.avgWfile')
standMethod <- function(object) object@avgWfile
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.avgWfile<-'
standGen <- function(x, value) standardGeneric('.model.avgWfile<-')
standMethod <- function(x, value) {x@avgWfile<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'model'
funcname <-'.model.trials'
standGen <- function(object) standardGeneric('.model.trials')
standMethod <- function(object) object@trials
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.model.trials<-'
standGen <- function(x, value) standardGeneric('.model.trials<-')
standMethod <- function(x, value) {x@trials<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.name'
standGen <- function(object) standardGeneric('.data.name')
standMethod <- function(object) object@name
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.name<-'
standGen <- function(x, value) standardGeneric('.data.name<-')
standMethod <- function(x, value) {x@name<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.fullpath'
standGen <- function(object) standardGeneric('.data.fullpath')
standMethod <- function(object) object@fullpath
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.fullpath<-'
standGen <- function(x, value) standardGeneric('.data.fullpath<-')
standMethod <- function(x, value) {x@fullpath<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.datafiles'
standGen <- function(object) standardGeneric('.data.datafiles')
standMethod <- function(object) object@datafiles
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.datafiles<-'
standGen <- function(x, value) standardGeneric('.data.datafiles<-')
standMethod <- function(x, value) {x@datafiles<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.weightfiles'
standGen <- function(object) standardGeneric('.data.weightfiles')
standMethod <- function(object) object@weightfiles
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.weightfiles<-'
standGen <- function(x, value) standardGeneric('.data.weightfiles<-')
standMethod <- function(x, value) {x@weightfiles<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.avgdatfile'
standGen <- function(object) standardGeneric('.data.avgdatfile')
standMethod <- function(object) object@avgdatfile
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.avgdatfile<-'
standGen <- function(x, value) standardGeneric('.data.avgdatfile<-')
standMethod <- function(x, value) {x@avgdatfile<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.avgWfile'
standGen <- function(object) standardGeneric('.data.avgWfile')
standMethod <- function(object) object@avgWfile
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.avgWfile<-'
standGen <- function(x, value) standardGeneric('.data.avgWfile<-')
standMethod <- function(x, value) {x@avgWfile<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
classname <-'data'
funcname <-'.data.trials'
standGen <- function(object) standardGeneric('.data.trials')
standMethod <- function(object) object@trials
setGeneric(funcname,standGen)
setMethod(funcname,classname,standMethod)
slotreplace <-'.data.trials<-'
standGen <- function(x, value) standardGeneric('.data.trials<-')
standMethod <- function(x, value) {x@trials<- value;x}
setGeneric(slotreplace,standGen)
setReplaceMethod(funcname,classname,standMethod)
