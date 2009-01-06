`displayEstimates` <-
function(arfmodel) {
	
	cat(' estimates:\n')
	for(i in 1:.model.regions(arfmodel)) cat('  ',i,': ',.model.estimates(arfmodel)[((1+(i-1)*6):(i*6))],'\n')
	
	
}

