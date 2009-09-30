\name{setFuncFiles}
\Rdversion{1.1}
\alias{setFuncFiles}
\title{Link functional files to functional volume.}
\description{
Link the functional files in a data directory to the functional volume files.
}
\usage{
setFuncFiles(experiment = .experiment, func_data = "filtered_func_data.nii.gz")
}
\arguments{
  \item{experiment}{Object of class "experiment"}
  \item{func_data}{Name of the functional volume in the 'func' directory.}
}
\value{
None 
}
\author{ Wouter D. Weeda - w.d.weeda@gmail.com }
\seealso{
	\code{\linkS4class{functional}}, \code{\linkS4class{experiment}}
}
\keyword{ methods }
\keyword{ models }

