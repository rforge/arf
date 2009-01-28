\name{fmri.data-class}
\docType{class}
\alias{fmri.data-class}


\title{Class "fmri.data" ~~~ }
\description{ Class containing fMRI data (and header info) in the NIFTI/ANALYZE format.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("fmri.data", ...)}.
	
}
\section{Slots}{
	 \describe{
    \item{\code{datavec}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sizeof_hdr}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{data_type}:}{Object of class \code{"character"} ~~ }
    \item{\code{db_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{extents}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{session_error}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{regular}:}{Object of class \code{"character"} ~~ }
    \item{\code{dim_info}:}{Object of class \code{"character"} ~~ }
    \item{\code{dims}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intent_p1}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intent_p2}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intent_p3}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intent_code}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{datatype}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{bitpix}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{slice_start}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{pixdim}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{vox_offset}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{scl_slope}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{scl_inter}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{slice_end}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{slice_code}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{xyzt_units}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{cal_max}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{cal_min}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{slice_duration}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{toffset}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{glmax}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{glmin}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{descrip}:}{Object of class \code{"character"} ~~ }
    \item{\code{aux_file}:}{Object of class \code{"character"} ~~ }
    \item{\code{qform_code}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{sform_code}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{quatern_b}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{quatern_c}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{quatern_d}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{qoffset_x}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{qoffset_y}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{qoffset_z}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{srow_x}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{srow_y}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{srow_z}:}{Object of class \code{"numeric"} ~~ }
    \item{\code{intent_name}:}{Object of class \code{"character"} ~~ }
    \item{\code{magic}:}{Object of class \code{"character"} ~~ }
    \item{\code{data.type}:}{Object of class \code{"character"} ~~ }
    \item{\code{data.signed}:}{Object of class \code{"logical"} ~~ }
    \item{\code{fullpath}:}{Object of class \code{"character"} ~~ }
    \item{\code{filename}:}{Object of class \code{"character"} ~~ }
    \item{\code{filetype}:}{Object of class \code{"character"} ~~ }
    \item{\code{extension}:}{Object of class \code{"character"} ~~ }
    \item{\code{gzipped}:}{Object of class \code{"logical"} ~~ }
    \item{\code{endian}:}{Object of class \code{"character"} ~~ }
  }
}
\section{Extends}{
Class \code{"\linkS4class{nifti.header}"}, directly.
Class \code{"\linkS4class{nifti.fileinfo}"}, by class "nifti.header".
}

\section{Methods}{
  \describe{
    \item{slots:}{Slots can be accessed via the standard accessor and replacment methods.}

	 }
}

\author{ Wouter D. Weeda }
 
\examples{

}

\keyword{classes}
