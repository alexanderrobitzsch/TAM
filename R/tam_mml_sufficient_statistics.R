## File Name: tam_mml_sufficient_statistics.R
## File Version: 0.04

tam_mml_sufficient_statistics <- function( nitems , maxK, resp, resp.ind ,
		pweights , cA, progress )
{  
    # more efficient calculation of sufficient statistics
    col.index <- rep( 1:nitems , each = maxK )
    cResp <- (resp +1) *resp.ind
    cResp <- cResp[ , col.index  ]
    cResp <- 1 * ( cResp == matrix( rep(1:(maxK), nitems) , nrow(cResp) , 
                                    ncol(cResp) , byrow=TRUE ) )
    cA[is.na(cA)] <- 0									
    if ( stats::sd(pweights) > 0 ){ 
		ItemScore <- as.vector( t( colSums( cResp * pweights ) ) %*% cA )
    } else { 
		ItemScore <- as.vector( t( colSums( cResp) ) %*% cA )			
    }
    if (progress){ 
		cat("    * Calculated Sufficient Statistics   (", 
        paste(Sys.time()) , ")\n")
		utils::flush.console()	  
    }    				   
    #--- OUTPUT
	res <- list(cResp=cResp, ItemScore=ItemScore, col.index=col.index)
	return(res)
}
