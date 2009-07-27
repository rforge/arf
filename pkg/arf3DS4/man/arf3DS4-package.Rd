\name{arf3DS4-package}
\Rdversion{1.1}
\alias{arf3DS4-package}
\alias{arf3DS4}
\docType{package}
\title{
Activated Region Fitting, fMRI data analysis (3D).
~~ package title ~~
}
\description{
Activated Region Fitting (ARF) is an analysis method for fMRI data. ARF works by fitting Gaussian shaped models to volumes (3D) of brain activity. Inference is based on the parameters of the estimated regions. ARF can read and write NIFTI data. ARF3DS4 supersedes ARFS4 and ARFS4lite
~~ A concise (1-5 lines) description of the package ~~
}
\details{
\tabular{ll}{
Package: \tab arf3DS4\cr
Type: \tab Package\cr
Version: \tab 1.1-8\cr
Date: \tab 2009-05-15\cr
Depends: \tab R (>= 2.8.0), methods\cr
URL: \tab http://arf.r-forge.r-project.org/\cr
License: \tab GNU (GPL)\cr
LazyLoad: \tab yes\cr
Repository: \tab R-Forge\cr
Repository/R-Forge/Project: \tab arf\cr
Repository/R-Forge/Revision: \tab 23\cr
Date/Publication: \tab 2009-05-15 12:56:07\cr
Packaged: \tab 2009-06-15 03:22:31 UTC; rforge\cr
Built: \tab R 2.9.0; universal-apple-darwin8.11.1; 2009-06-16 02:47:32 UTC; unix\cr
}

Index:
\preformatted{
arf                     ~~function to do ... ~~
arf3DS4-package         Analysis of fMRI data using parameterized
                        regions of activation.
arfSim                  ~~function to do ... ~~
blobMe                  ~~function to do ... ~~
data-class              S4 data class
experiment-class        S4 experiment class
fitModel                fit an ARF model to data
fmri.data-class         S4 fmri.data class
loadData                load data
loadExp                 load an ARF Experiment
loadRda                 load a Rda file to an object
makeExp                 Make an ARF experiment structure based on
                        existing directories
makeExpDirs             Create an ARF experiment directory structure
model-class             S4 model class
newModel                create a new ARF model
options-class           S4 options class
readData                read in NIFTI/ANALYZE data
sequence-class          S4 sequence class
simBlobs3D              ~~function to do ... ~~
wald-class              S4 wald class
writeData               write data in NIFTI/ANALYZE format
}
~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Wouter D. Weeda

Maintainer: Wouter D. Weeda <w.d.weeda@gmail.com>
~~ The author and/or maintainer of the package ~~
}
\references{
~~ Literature or other references for background information ~~
}
~~ Optionally other standard keywords, one per line, from file KEYWORDS in the R documentation directory ~~
\keyword{ package }
\seealso{
~~ Optional links to other man pages, e.g. ~~
~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
}
\examples{
~~ simple examples of the most important functions ~~
}
