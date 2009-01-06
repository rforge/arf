`displayFitEnd` <-
function(arfmodel,time1) {
	
	
	cat(' ',.model.convergence(arfmodel),'\n')
	
	td=list(dif=0,un='none')
	
	time2 <- as.POSIXlt(Sys.time())
	diff.time <- difftime(time2,time1,units='sec')
	
	if(diff.time<60) {td$dif=diff.time;td$un='second(s)'}
	if(diff.time>=60 & diff.time<=3600) {td$dif=diff.time/60;td$un='minute(s)'}
	if(diff.time>3600) {td$dif=diff.time/3600;td$un='hours(s)'}
		
	cat(' process time was',round(td$dif,3),td$un,'\n',sep=' ')
	
}

