\name{fitSimpleModelNlm}
\Rdversion{1.1}
\alias{fitSimpleModelNlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
fitSimpleModelNlm(arfmodel, options = loadOptions(arfmodel), dat = readData(.model.avgdatfile(arfmodel)), weights = readData(.model.avgWfile(arfmodel)), printlevel = 0, try.silen = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfmodel}{
%%     ~~Describe \code{arfmodel} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
}
  \item{dat}{
%%     ~~Describe \code{dat} here~~
}
  \item{weights}{
%%     ~~Describe \code{weights} here~~
}
  \item{printlevel}{
%%     ~~Describe \code{printlevel} here~~
}
  \item{try.silen}{
%%     ~~Describe \code{try.silen} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function(arfmodel,options=loadOptions(arfmodel),dat=readData(.model.avgdatfile(arfmodel)),weights=readData(.model.avgWfile(arfmodel)),printlevel=0,try.silen=T) {

	sp <- .Platform$file.sep
	
	.options.min.routine(options) <- 'nlm'
	.model.modeltype(arfmodel) <- 'simple'
	.model.params(arfmodel) <- 5
		
	#start_time
	st_time <- Sys.time()

	#check if averages exist
	if(!file.exists(.model.avgdatfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	if(!file.exists(.model.avgWfile(arfmodel))) stop('Averages do not exist, please run createAverages')
	
	#clear the warnings and deriv + residualfilres
	.model.warnings(arfmodel) <- character(0)
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.residualFile(arfmodel),sep=''))
	if(file.exists(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))) file.remove(paste(.model.modeldatapath(arfmodel),sp,.model.derivativeFile(arfmodel),sep=''))
	.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),'Simple Gaussmodel was fitted.')
	
	#load startingvalues
	.model.startval(arfmodel) <- loadStart(arfmodel)
	
	#call NLM (within a try-loop)
	nlm.output <- try(suppressWarnings(nlm(
							ssq.simple,
							.model.startval(arfmodel),
							datavec=.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],
							weightvec=.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],
							np=.model.regions(arfmodel)*.model.params(arfmodel),
							brain=.model.mask(arfmodel),
							dimx=.fmri.data.dims(dat)[2],
							dimy=.fmri.data.dims(dat)[3],
							dimz=.fmri.data.dims(dat)[4],
							ss_data=.model.ss(arfmodel),
							analyticalgrad=.options.min.analyticalgrad(options),
							print.level=printlevel,
							hessian=F,
							check.analyticals=F,
							iterlim=.options.min.iterlim(options),
							gradtol=.options.nlm.gradtol(options),
							steptol=.options.nlm.steptol(options)
					)),silen=try.silen)
	
	#end_time
	en_time <- Sys.time()
	
	# check for internal errors and set relevant arf model values
	if(is.null(attr(nlm.output,'class'))) {
		if(nlm.output$code==1) .model.convergence(arfmodel) <- paste('Gradient close to zero. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==2) .model.convergence(arfmodel) <- paste('Iterates within tolerance. Converged in ',nlm.output$iterations,' iterations.',sep='')
		if(nlm.output$code==3) .model.convergence(arfmodel) <- 'No lower point found.' 
		if(nlm.output$code==4) .model.convergence(arfmodel) <- 'Iteration limit exceeded. No convergence.'
		if(nlm.output$code==5) .model.convergence(arfmodel) <- 'Stepmax exceeded five times. No convergence.'
		if(nlm.output$code <= 2) .model.valid(arfmodel) <- TRUE else .model.valid(arfmodel) <- FALSE
	
		#set model essentials
		.model.estimates(arfmodel) <- nlm.output$estimates
		.model.iterates(arfmodel) <- nlm.output$iterations
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		
		if(.model.valid(arfmodel)) {
			#save the ModelBinary
			arfmodel <- saveModelBinSimple(arfmodel)
			
			#set model objects
			.model.minimum(arfmodel) <- nlm.output$minimum
			.model.estimates(arfmodel) <- rep(0,.model.regions(arfmodel)*10)
			for(i in 1:.model.regions(arfmodel)) {
				.model.estimates(arfmodel)[1+(10*(i-1))] <- nlm.output$estimate[1+(5*(i-1))]
				.model.estimates(arfmodel)[2+(10*(i-1))] <- nlm.output$estimate[2+(5*(i-1))]
				.model.estimates(arfmodel)[3+(10*(i-1))] <- nlm.output$estimate[3+(5*(i-1))]
				.model.estimates(arfmodel)[4+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[5+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[6+(10*(i-1))] <- nlm.output$estimate[4+(5*(i-1))]
				.model.estimates(arfmodel)[7+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[8+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[9+(10*(i-1))] <- 0
				.model.estimates(arfmodel)[10+(10*(i-1))] <- nlm.output$estimate[5+(5*(i-1))]
			}
			
			if(.options.min.analyticalgrad(options)) .model.gradient(arfmodel) <- gradient.simple(.model.estimates(arfmodel),.fmri.data.datavec(dat)[1:(.fmri.data.dims(dat)[2]*.fmri.data.dims(dat)[3]*.fmri.data.dims(dat)[4])],.fmri.data.datavec(weights)[1:(.fmri.data.dims(weights)[2]*.fmri.data.dims(weights)[3]*.fmri.data.dims(dat)[4])],.model.mask(arfmodel),.model.regions(arfmodel)*.model.params(arfmodel),.fmri.data.dims(dat)[2],.fmri.data.dims(dat)[3],.fmri.data.dims(dat)[4],.model.ss(arfmodel),analyticalgrad=T)
			
					
			if(.model.valid(arfmodel)) {
				arfmodel = BIC(arfmodel,options=options)
				arfmodel = RMSEA(arfmodel,options=options)
			}
		}
		
	} else {
		.model.convergence(arfmodel) <- 'Internal error, no convergence.'
		.model.warnings(arfmodel) <- c(.model.warnings(arfmodel),optim.output)
		.model.proctime(arfmodel)[1,1] <- as.numeric(difftime(en_time,st_time,units='sec'))
		.model.valid(arfmodel) <- FALSE
	}
	
	if(!.model.valid(arfmodel)) .model.warnings(arfmodel) <- c(.model.warnings(arfmodel),.model.convergence(arfmodel)) 
	
	#save the modelInfo
	saveModel(arfmodel)
	
	#return arf model object	
	return(invisible(arfmodel))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
