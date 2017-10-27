## File Name: summary.msq.itemfit.R
## File Version: 9.09

###################################################
# summary for tam.fit
summary.msq.itemfit <- function( object , file=NULL,  ... ){

	tam_osink( file = file)

	cat("------------------------------------------------------------\n")

	#- package and R session
	tam_print_package_rsession(pack="TAM")			
	#- computation time
	tam_print_computation_time(object=object)
	
	cat("MSQ item fit statitics (Function 'msq.itemfit')\n\n")

	cat("****************************************************\n")
	cat("\nSummary outfit and infit statistic\n")
    obji <- object$summary_itemfit
	rownames(obji) <- NULL
	for ( vv in seq(2,ncol(obji) ) ){
		obji[,vv] <- round( obji[,vv] , 3 )
	}		
	print(obji)
	
	cat("\n****************************************************\n")
	cat("\nOutfit and infit statistic\n")	
    object <- object$itemfit
	ind <- grep( "fitgroup" , colnames(object) )
	obji <- object
	for ( vv in seq(ind+1,ncol(obji) ) ){
		obji[,vv] <- round( obji[,vv] , 3 )
	}
	print(obji)
	invisible(obji)
	
	tam_csink(file=file)
}
###################################################
