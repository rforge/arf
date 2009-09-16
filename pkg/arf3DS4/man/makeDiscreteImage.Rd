\name{makeDiscreteImage}
\Rdversion{1.1}
\alias{makeDiscreteImage}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
makeDiscreteImage(datavec, zerotol = 1e-06)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datavec}{
%%     ~~Describe \code{datavec} here~~
}
  \item{zerotol}{
%%     ~~Describe \code{zerotol} here~~
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
function(datavec,zerotol=1e-06)
{
	#define maxsteps
	maxsteps = 64
	
	datavec[abs(datavec)<zerotol]=0
	
	
	max_dat = max(datavec)
	min_dat = min(datavec)
	total = abs(min_dat)+abs(max_dat)
	
	possteps = round(maxsteps*(abs(max_dat)/total))
	negsteps = round(maxsteps*(abs(min_dat)/total))
	
	pos_data = datavec[datavec>0]
	neg_data = datavec[datavec<0]
	zero_data = datavec[datavec==0]
		
	if(max_dat>0 & min_dat<0) {
		pq = quantile(pos_data,probs=seq(0,1,1/possteps)[-1])
		nq = quantile(neg_data,probs=seq(1,0,-1/negsteps)[-1])
	}
	
	if(max_dat>0 & min_dat>=0) {
		possteps = maxsteps
		pq = quantile(pos_data,probs=seq(0,1,1/possteps)[-1])
		nq = numeric(0)
	}
	
	if(max_dat<=0 & min_dat<0) {
		negsteps = maxsteps
		nq = quantile(neg_data,probs=seq(1,0,-1/maxsteps)[-1])
		pq = numeric(0)
	}
	
	if(max_dat==0 & min_dat==0) {
		nq = numeric(0)
		pq = numeric(0)
	}
		
	newdata=rep(NA,length(datavec))
	
	if(length(pq)>0) newdata[datavec>0 & datavec<pq[1]]=1
	if(length(nq)>0) newdata[datavec<0 & datavec>nq[1]]=-1
	
	if(length(pq)>0) for(i in 1:possteps) newdata[datavec>=pq[i]]=i+1
	if(length(nq)>0) for(i in 1:negsteps) newdata[datavec<=nq[i]]=-i-1
	
	newdata[datavec==0]=0
	
	return(newdata)
	
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
