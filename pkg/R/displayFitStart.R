`displayFitStart` <-
function(arfmodel) {
	
	cat('[',.model.modelname(arfmodel),']','\n',sep='')
	cat(' fitting ',.model.regions(arfmodel),' region(s)\n',sep='')
	cat(' starting values:\n')
	for(i in 1:.model.regions(arfmodel)) cat('  ',i,': ',.model.startval(arfmodel)[((1+(i-1)*6):(i*6))],'\n')
	
	time1=as.POSIXlt(Sys.time())
	
	if(round(time1$sec)<10) secs <- paste('0',round(time1$sec),sep='') else secs <- round(time1$sec)
	if(time1$min<10) mins <- paste('0',time1$min,sep='') else mins <- time1$min
	if(time1$hour<10) hours <- paste('0',time1$hour,sep='') else hours <- time1$hour
	
	if(is.na(secs)) secs <- '00'
	if(is.na(mins)) mins <- '00'
	if(is.na(hours)) hours <- '00'
	
	cat(' iterations started at: ',paste(hours,':',mins,':',secs,sep=''),'\n',sep='')
		
	return(time1)
}

