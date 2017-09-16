## File Name: summary.msq.itemfitWLE.R
## File Version: 9.09
## File Last Change: 2017-09-16 13:43:23

###################################################
# summary for objects of class msq.itemfitWLE
summary.msq.itemfitWLE <- function( object , file=NULL, ... ){

	tam_osink( file = file )

	cat("------------------------------------------------------------\n")

	#- package and R session
    tam_print_package_rsession(pack="TAM")			
	#- computation time
	tam_print_computation_time(object=object)	

	cat("MSQ item fit statitics (Function 'msq.itemfitWLE')\n\n")

	cat("****************************************************\n")
	cat("\nSummary outfit and infit statistic\n")
	
	if ( is.null(object$fitindices) ){
		object1 <- object$fit_data_summary 
				} else {
		object1 <- object$fit_parm_summary 
					}	
	
    obji <- object1
	rownames(obji) <- NULL
	
	for ( vv in seq(2,ncol(obji) ) ){
		obji[,vv] <- round( obji[,vv] , 3 )
	}	
	
	print(obji)
	
	cat("\n****************************************************\n")
	cat("\nOutfit and infit statistic\n")		
		
	if ( is.null(object$fitindices) ){
		object <- object$fit_data 
	} else {
		object <- object$fit_parm 
	}
	
	obji <- object
	rownames(obji) <- NULL
	for ( vv in seq(2,ncol(obji) ) ){
		obji[,vv] <- round( obji[,vv] , 3 )
	}
    
	print(obji)
	
	tam_csink(file=file)
}
###################################################
