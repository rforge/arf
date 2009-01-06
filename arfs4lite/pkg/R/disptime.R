`disptime` <-
function(timeobject) paste(as.POSIXlt(timeobject)$hour,':',as.POSIXlt(timeobject)$min,':',as.POSIXlt(timeobject)$sec,sep='')

