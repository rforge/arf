\name{arf3DS4-package}
\Rdversion{1.1}
\alias{arf3DS4-package}
\alias{arf3DS4}
\docType{package}
\title{
Activated Region Fitting (ARF), fMRI data analysis.
}
\description{
Activated Region Fitting (ARF) is an analysis method for fMRI data. The method uses Gaussian shaped spatial models to model active brain regions. Parameters of the spatial models are then used to test hypothesis on location, spatial extent, and amplitude of each region. ARF3DS4 uses the beta-values or t-values from a GLM analysis, these can be in NIFTI or ANALYZE format. The package is compatible with FSL (FMRIB), and FSLView. Details of the method can be found in the manual. 
}
\details{
\tabular{ll}{
Package: \tab arf3DS4\cr
Type: \tab Package\cr
Version: \tab 1.7-5\cr
Date: \tab 2009-11-02\cr
Depends: \tab R (>= 2.9.1), methods\cr
URL: \tab http://arf.r-forge.r-project.org/\cr
License: \tab GNU (GPL)\cr
LazyLoad: \tab yes\cr
Repository: \tab R-Forge\cr
Repository/R-Forge/Project: \tab arf\cr
Repository/R-Forge/Revision: \tab 97\cr
Publication: \tab 2009-11-02 16:31:15\cr
Packaged: \tab 2009-11-02 16:31:15; rforge\cr
Built: \tab R 2.9.1; universal-apple-darwin8.11.1; 2009-11-02 16:31:15; unix\cr
}
To get a feeling of the arf3DS4 package, it is recommended to read the manual first. More details can also be found in the ARF paper (see reference section).
arf3DS4 stores all its data in a predefined directory structure, this is mainly because of the size of fMRI data files. To save memory the data is often not stored directly in an R object, but the location where the files can be found is stored instead. 
Information of the directory structure is stored in the S4 class experiment and can be customized (by adjusting the S4 settings class). The fmri data files are stored in the S4 class fmri.data (wich includes the classes nifti.header and nifti.fileinfo). A set of fmri data files belonging to one condition in the experiment are stored in the S4 class data. To this data an ARF model can be fitted, the model information is stored in the S4 class model. Setting the options of the model fitting procedure etc. is done via the S4 class options. A valid ARF model contains hypothesis tests of its parameters, these are in the S4 class wald. Finally an ARF model can be registered to standard MNI space via the S4 registration class and to the functional data via the S4 functional class. Slots of the classes can be accessed and replaced using standard accessor and replacement functions. The following functions are user-level: 
\preformatted{
*** ARF3DS4 USER LEVEL FUNCTIONS: ***
Experiment functions:
setExp              set an experiment based on files in the data directories
makeExpDirs         make an ARF directory structure
loadExp             load an ARF experiment
getFSL              copy FSL data to an ARF experiment
fMRI data functions:
getFileInfo         get information of a NIFTI/ANALYZE file
readData            read NIFTI/ANALYZE data
writeData           write NIFTI/ANALYZE data
cropVolume          crop a NIFIT/ANALYZE volume
createRegs          create registration directories (for use with standard brain atlases)
setRegFiles         set registration files (for use with standard brain atlases)
setRegParams        set transformation matrices based on registration files (for use with standard brain atlases)
createFuncs         create functional directories (not yet implemented)
arfToMNI            convert ARF native coordinates to MNI standard coordinates
ARF model functions:
fitModelNlm         fit an ARF model with nlm()
fitModelOptim       fit an ARF model with optim()
fitModel            wrapper for fitModelNlm and fitModelOptim
fitSimpleModelNlm   fit a simplified ARF model with nlm()
fitSimpleModelOptim fit a simplified ARF model with optim()
processModel        process an ARF model (determine start, fit, analyze)
createAverages      create average maps based on trial data
createAllAverages   create all averages in an ARF experiment
determineStartRect  determine starting values for ARF based on a rectangular search
newModel            create a new ARF model based on ARF data
saveModel           save the ARF model
loadModel           load ARF model files
Generic arf functions:
loadOptions         load an ARF options file
loadStart           load starting values from an ARF model
loadReg             load registration files
loadRda             load .Rda files	
loadData            load ARF data files
Variance/covariance, fit and statistics related functions:
makeDerivs          make analytical derivatives
makeResiduals       Make residuals
varcov              calculate the variance/covariance matrix
BIC                 calculate the BIC
RMSEA               calculate the RMSEA
mcpCorrect          calculate standard multiple comparison corrections 
wald                calculate the wald statistics
}
}
\note{
arf3DS4 supersedes arfS4 and arfS4lite. 
}
\author{
Wouter D. Weeda
Maintainer: Wouter D. Weeda <w.d.weeda@gmail.com>
}
\references{
Wouter D. Weeda, Lourens J. Waldorp, Ingrid Christoffels and Hilde M. Huizenga. (2009) Activated Region Fitting: A robust high-power method of fMRI analysis using parameterized regions of activation. Human Brain Mapping, 30(8), 2595-2605.
}
\keyword{ package }
