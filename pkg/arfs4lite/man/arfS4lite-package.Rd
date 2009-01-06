\name{arfS4lite-package}
\alias{arfS4lite-package}
\alias{arfS4lite}
\docType{package}
\title{
Activated Region Fitting: fMRI data analysis
}
\description{
ARF fits Gaussian shaped models on fMRI activation data. Robust hypothesis tests are performed on the parameters of these models. 
}
\details{
\tabular{ll}{
Package: \tab arfS4lite\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2008-09-30\cr
License: \tab GNU (GPL)\cr
LazyLoad: \tab yes\cr
}
Since fMRI data is often very large the data are not loaded into memory at runtime. Rather the fmri data objects ARF uses point to the files where the data are stored. ARF uses beta-values and standard-errors of the beta-values of several independent from a fMRI GLM analysis. The files containing beta-values are assumed to be stored in a '/data' directory and the standard-errors are assumed to be stored in a '/weights' directory. 
}
\author{
Wouter Weeda

Maintainer: Wouter Weeda <w.d.weeda@gmail.com>

}
\references{
Weeda, W.D., Waldorp, L.J., Christoffels, I., and Huizenga, H.M. (submitted). Activated Region Fitting: A robust, high power method for fMRI analysis. Human Brain Mapping.
}

\keyword{ package }
\seealso{

}
\examples{

}
