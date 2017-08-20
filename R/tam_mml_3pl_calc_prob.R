
#####################################################################
# calc_prob
# Calculation of probabilities
tam_mml_3pl_calc_prob <- function(iIndex, A, AXsi, B, xsi, theta, 
           nnodes, maxK, recalc=TRUE , guess, subtract_max = TRUE )
{		   

    if(recalc){
		LI <- length(iIndex)
		LXsi <- dim(A)[3]
		AXsi.tmp <- array( 0 , dim = c( LI , maxK , nnodes ) )
		for (kk in 1:maxK){
			A_kk <- matrix( A[ iIndex , kk , ] , nrow = LI , ncol = LXsi )
			AXsi.tmp[, kk , 1:nnodes ] <- A_kk %*% xsi
		}		 						 
		AXsi[iIndex,] = AXsi.tmp[,,1]
    } else {
		AXsi.tmp <- array( AXsi, dim = c( length(iIndex) , maxK , nnodes ) )
    }

    Btheta <- array(0, dim = c(length(iIndex) , maxK , nnodes) )
    for( dd in 1:ncol(theta) ){  
		Btheta <- Btheta + array(B[iIndex,,dd ,drop = FALSE] %o% theta[,dd] , dim = dim(Btheta))
	}
							
    # rprobs <- ( rr <- exp(Btheta+AXsi.tmp) )/aperm( array( rep( colSums( aperm( rr , c(2,1,3) ) ,
	#				dims=1 , na.rm = TRUE) ,    maxK ), dim=dim(rr)[c(1,3,2)] ) , c(1,3,2) )

	#*** subtract maximum and use in Rcpp
	rr0 <- Btheta + AXsi.tmp	
	if ( subtract_max){
		rr1 <- tam_calc_prob_helper_subtract_max( rr0=rr0 )
	} else {
		rr1 <- rr0
	}
	
	rr <- exp(rr1)		
    rprobs <- rr / aperm( array( rep( colSums( aperm( rr , c(2,1,3) ) ,
					dims=1 , na.rm = TRUE) ,  maxK ), dim=dim(rr)[c(1,3,2)] ) , c(1,3,2) )							
			
	# include guessing	
	rprobs0 <- rprobs
	
	ind <- which(guess[ iIndex ] > 1E-6 )
	if ( length(ind) > 0 ){
		rprobs[ ind , 2 , ] <- guess[ind] + ( 1-guess[ind] ) * rprobs0[ind,2,]	
		rprobs[ ind , 1 , ] <- 1 - rprobs[ ind , 2 , ]
	}						
	
    return(list("rprobs" = rprobs, "AXsi" = AXsi , "rprobs0"=rprobs0 ))
}
##############################################################################

.mml.3pl.calc_prob.v5 <- tam_mml_3pl_calc_prob
