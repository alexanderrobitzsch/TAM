


#**********************************************************************
tam_wle_print_general <- function( ndim , nobs, CALL )
{
	#*** print output with general informations
    cat("Object of class 'tam.wle'\nCall: ")
    print( CALL ) 
    # cat("\n")	
	if (ndim==1){ 
		D0 <- ""
	} else { 
		D0 <- "s" 
	}	
    v1 <- paste0("\n  WLEs for ", nobs , " observations and " ,
		    ndim , " dimension" , D0 , "\n")
	cat(v1)
}
