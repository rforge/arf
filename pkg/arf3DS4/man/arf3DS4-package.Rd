\name{arf3DS4-package}
\alias{arf3DS4-package}
\alias{arf3DS4}
\docType{package}
\title{
Analysis of fMRI data using parameterized regions of activation.

}
\description{
Activated Region Fitting (ARF) analyzes images of fMRI activation by fitting multiple Gaussian shaped models on activated areas. Hypothesis tests are performed on the parameters of the model allowing for hypotheses on location, spatial extent and amplitude.  
}
\details{
\tabular{ll}{
Package: \tab arf3DS4\cr
Type: \tab Package\cr
Version: \tab 1.1-6\cr
Date: \tab 2009-04-09\cr
License: \tab GPL\cr
LazyLoad: \tab yes\cr
Depends: \tab methods\cr
}
ARF uses multiple measures of a condition (either from a blocked design or an event related design) as input. It requires for each image a file containing beta-values from the GLM analysis and a separate file containing the standard errors of the beta-values (note that t-values with s.e. set to one can also be used). ARF assumes a directory structure with for each condition a separate directory with subdirectories 'data' and 'weights' containing the beta-values and the standard errors respectively. 'MakeExpDirs' automatically creates a directory structure given a name for the condition. After the files are in place a call to 'arf by default creates the average beta-images and fits the specified model.
}
\author{
Maintainer: Wouter D. Weeda <w.d.weeda@gmail.com>

}
\references{
Weeda, W.D., Waldorp, L.J., Christoffels, I., and Huizenga, H.M. (in press) Activated Region Fitting: A robust high-power method for fMRI analysis using parameterized regions of activation. Human Brain Mapping, 2009.
}

\keyword{ package }
