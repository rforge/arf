\name{getFSLdata}
\Rdversion{1.1}
\alias{getFSLdata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getFSLdata(fsldir = getwd(), subjectlist, expname = "FSLtoARF", expdir = getwd(), settings = new("settings"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fsldir}{
%%     ~~Describe \code{fsldir} here~~
}
  \item{subjectlist}{
%%     ~~Describe \code{subjectlist} here~~
}
  \item{expname}{
%%     ~~Describe \code{expname} here~~
}
  \item{expdir}{
%%     ~~Describe \code{expdir} here~~
}
  \item{settings}{
%%     ~~Describe \code{settings} here~~
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
function(fsldir=getwd(),subjectlist,expname='FSLtoARF',expdir=getwd(),settings=new('settings')) 
{
	#set separator
	sp = .Platform$file.sep
	
	cat('getFSLdata featdir',fsldir,'\n')
	cat('getFSL for subjects:\n')
	for(i in 1:length(subjectlist)) cat(' >',subjectlist[i],'\n')
	
	#standard FSL files DO NOT CHANGE!!!!
	functional = c('filtered_func_data.nii.gz')
	registration = c('example_func2standard.mat','example_func2highres.mat','highres2standard.mat','example_func.nii.gz','highres.nii.gz','standard.nii.gz')
	cope = 'cope'
	varcope = 'varcope'
	
	subjects <- length(subjectlist)
	subinf <- vector(subjects,mode='list')
	
	cat('fetching fsl files...')
	#get all subject files and info for copying
	for(subj in 1:subjects) {
			
		dirs = list.files(fsldir,subjectlist[subj],full=T)
		trials = length(dirs)
		
		func_file=reg_file=cope_file=vector(trials,mode='list')
				
		for(tri in 1:trials) {
		
			#get functional data
			func_file[[tri]] = list.files(dirs[tri],functional,full=T)
			
			#get registration data
			rdir = paste(dirs[tri],sp,'reg',sep='')
			reg_file[[tri]] = list(ex2stand=list.files(rdir,registration[1],full=T),ex2hi=list.files(rdir,registration[2],full=T),hi2st=list.files(rdir,registration[3],full=T),ex=list.files(rdir,registration[4],full=T),hi=list.files(rdir,registration[5],full=T),st=list.files(rdir,registration[6],full=T))
			
			reg_file[[tri]]$hi = reg_file[[tri]]$hi[-1]
			reg_file[[tri]]$st = reg_file[[tri]]$st[-c(1,2)]
			
			
			#get copes and varcopes
			sdir = paste(dirs[tri],sp,'stats',sep='')
			all_copes = list.files(sdir,cope,full=T)
			vcs = grep(varcope,all_copes)
			
			#get varcopes out of it and put all in list 
			cope_file[[tri]] = list(copes=all_copes[-vcs],varcopes=all_copes[vcs])
		}
		
		subinf[[subj]] = list(func_file=func_file,reg_file=reg_file,cope_file=cope_file,trials=trials,contrasts=length(cope_file[[1]]$copes))
	
	}
	cat('ok\n')
	
	cat('making experiment dirs...')
	#make new experiment
	experiment <- makeExpDirs(path=expdir,name=expname,subjectind=subjectlist,conditionind=paste('contrast',(1:subinf[[1]]$contrasts),sep=''),settings=settings)
	subd <- paste(.experiment.path(experiment),sp,.settings.subjectDir(settings),sep='')
	cat('ok\n')
	
	cat('copying images...')
	#fill the experiment with files
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		#get to subjectsdir
		sn <- paste(subd,sp,.settings.subjectPrefix(settings),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.settings.conditionDir(settings),sep='')
			
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			#get to conditiondir and datadir
			cn <- paste(subc,sp,.settings.conditionPrefix(settings),.experiment.condition.names(experiment)[cdirs],sep='')
			dn <- paste(cn,sp,.settings.dataDir(settings),sep='')
			
			#get the trials for the conditions 
			for(i in 1:subinf[[sdirs]]$trials) {
				copefile = avgfile = readData(subinf[[sdirs]]$cope_file[[i]]$copes[cdirs])
				varcopefile = readData(subinf[[sdirs]]$cope_file[[i]]$varcopes[cdirs])
				tstat = .fmri.data.datavec(copefile)/sqrt(.fmri.data.datavec(varcopefile))
				tstat[is.nan(tstat)]=0
				.fmri.data.fullpath(avgfile) = paste(dn,sp,.settings.betaDir(settings),sep='')
				.fmri.data.filename(avgfile) = paste('arf_tstat',i,sep='')
				.fmri.data.descrip(avgfile) = 'arf t_stat image'
				writeData(avgfile,tstat)
			}
			
			cropVolumeAuto(paste(dn,sp,.settings.betaDir(settings),sep=''),quiet=T)
		}
	}
	cat('ok\n')
	
	cat('setting experiment...')
	#set experiment and create weights
	experiment <- setExp(.experiment.path(experiment),1,1,T,T,T,F)
	cat('ok\n')
	
	cat('setting registration and functional files...')
	#create and fill regs and funcs
	for(sdirs in 1:.experiment.subject.num(experiment)) {
		
		#get to subjectsdir
		sn <- paste(subd,sp,.settings.subjectPrefix(settings),.experiment.subject.names(experiment)[sdirs],sep='')
		subc <- paste(sn,sp,.settings.conditionDir(settings),sep='')
		
		for(cdirs in 1:.experiment.condition.num(experiment)) {
			
			#get to conditiondir and datadir
			cn <- paste(subc,sp,.settings.conditionPrefix(settings),.experiment.condition.names(experiment)[cdirs],sep='')
			dn <- paste(cn,sp,.settings.dataDir(settings),sep='')
			
			arfdata <- loadRda(paste(dn,sp,.settings.dataRda(settings),sep=''))
			createRegs(arfdata)
			createFuncs(arfdata)
			
			filelist_reg = list.files(paste(dn,sp,.settings.regDir(settings),sep=''))
			filelist_func = list.files(paste(dn,sp,.settings.funcDir(settings),sep=''))
			
			for(i in 1:subinf[[sdirs]]$trials) {
				
				registration <- loadRda(paste(dn,sp,.settings.regDir(settings),sp,filelist_reg[i],sp,.settings.regRda(settings),sep=''))
			
				
				for(f in 1:length(subinf[[sdirs]]$reg_file[[i]])) {
					fn = as.character(subinf[[sdirs]]$reg_file[[i]][f])
					fns = strsplit(fn,sp)[[1]]
					file.copy(fn,paste(.registration.fullpath(registration),sp,fns[length(fns)],sep=''))
					
				}
				
				registration <- setRegFiles(registration)
				registration <- setRegParams(registration)
				
				functional <- loadRda(paste(dn,sp,.settings.funcDir(settings),sp,filelist_func[i],sp,.settings.funcRda(settings),sep=''))
				browser()
				fn = as.character(subinf[[sdirs]]$func_file[[i]][1])
				fns = strsplit(fn,sp)[[1]]
				file.copy(fn,paste(.functional.fullpath(functional),sp,fns[length(fns)],sep=''))
			
				functional <- setFuncFile(functional) 
						
			}
		}
	}
	cat('ok\n')
	
	#set experiment and create weights
	experiment <- setExp(.experiment.paht(experiment),1,1,T,T,T,T)
	
	return(invisible(experiment))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
