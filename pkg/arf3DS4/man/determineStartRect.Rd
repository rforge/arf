\name{determineStartRect}
\Rdversion{1.1}
\alias{determineStartRect}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
determineStartRect(arfmodel, options = loadOptions(arfmodel))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arfmodel}{
%%     ~~Describe \code{arfmodel} here~~
}
  \item{options}{
%%     ~~Describe \code{options} here~~
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
function(arfmodel,options=loadOptions(arfmodel)) 
# determineStartRect calculates starting values for regions (rectangular mode)
{
	
	.model.modeltype(arfmodel) <- 'gauss'
	.model.params(arfmodel) <- 10
	
	#load in fmriData
	fmridata <- readData(.model.avgtstatFile(arfmodel))
	
	#set theta to the default values (for all regions)
	theta <- rep(.options.start.vector(options),.model.regions(arfmodel))
	
	#set dimensions and read in data
	dimx <- .fmri.data.dims(fmridata)[2]
	dimy <- .fmri.data.dims(fmridata)[3]
	dimz <- .fmri.data.dims(fmridata)[4]
	data <- .fmri.data.datavec(fmridata)[1:(dimx*dimy*dimz)]
	
	mindim=c(1,1,1)
	maxdim=c(dimx,dimy,dimz)
	min_amp = .options.start.vector(options)[10]*-1
	max_amp = .options.start.vector(options)[10]
	
	#set dims of the data
	dim(data) <- c(dimx,dimy,dimz)
	
	#set location matrix
	location <- data.frame(x=rep(seq(1:dimx),times=dimz*dimy),y=rep(rep(seq(1:dimy),each=dimx),times=dimz),z=rep(seq(1:dimz),each=dimx*dimy))
	
	#set mask
	mask <- .model.mask(arfmodel)
	dim(mask) <-  c(dimx,dimy,dimz)
	
	for(reg in 1:.model.regions(arfmodel)) {
		
		#retrieve location in x,y,z
		m <- location[which.max(abs(data)),]
		if(data[m$x,m$y,m$z]<0) theta[10+(10*(reg-1))]=min_amp else theta[10+(10*(reg-1))]=max_amp
		
		#set maximum locations
		theta[1+(10*(reg-1))] <- m$x
		theta[2+(10*(reg-1))] <- m$y
		theta[3+(10*(reg-1))] <- m$z
				
		#caluclatefalloff
		xf <- fallOff(data[,m$y,m$z],.options.start.maxfac(options))
		yf <- fallOff(data[m$x,,m$z],.options.start.maxfac(options))
		zf <- fallOff(data[m$x,m$y,],.options.start.maxfac(options))
		
		#set width in x,y and z dirs
		theta[4+(10*(reg-1))] <- round(mean(xf))
		theta[5+(10*(reg-1))] <- round(mean(yf))
		theta[6+(10*(reg-1))] <- round(mean(zf))
		
		#check widths for zeroes
		theta[4+(10*(reg-1))][theta[4+(10*(reg-1))]<=0]=1 
		theta[5+(10*(reg-1))][theta[5+(10*(reg-1))]<=0]=1 
		theta[6+(10*(reg-1))][theta[6+(10*(reg-1))]<=0]=1 
		
		#zero tha data
		xvec <- (m$x-xf[1]):(m$x+xf[2])
		yvec <- (m$y-yf[1]):(m$y+yf[2])
		zvec <- (m$z-zf[1]):(m$z+zf[2])
		
		rmx <- which(xvec<mindim[1] | xvec>maxdim[1])
		rmy <- which(yvec<mindim[2] | yvec>maxdim[2])
		rmz <- which(zvec<mindim[3] | zvec>maxdim[3])
		
		if(length(rmx)>0 & length(rmx)<length(xvec)) xvec <- xvec[-rmx]
		if(length(rmy)>0 & length(rmy)<length(yvec)) yvec <- yvec[-rmy]
		if(length(rmz)>0 & length(rmz)<length(zvec)) zvec <- zvec[-rmz]	
		
		data[xvec,yvec,zvec]=0

	}
	
	
	#save startingvalues
	.model.startval(arfmodel) <- theta
	saveStart(.model.startval(arfmodel),arfmodel)
	
	#save startmap
	.fmri.data.fullpath(fmridata) <- .model.modeldatapath(arfmodel)
	.fmri.data.filename(fmridata) <- 'startmap'
	.fmri.data.intent_name(fmridata) <- 'start_search_map'
	writeData(fmridata,as.vector(data))
	
	#save model
	saveModel(arfmodel)
	
	return(invisible(arfmodel))
	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
