## File Name: tam_mml_calc_prob.R
## File Version: 9.23
## File Last Change: 2017-05-10 11:45:04

#####################################################################
# calc_prob
# Calculation of probabilities
tam_mml_calc_prob <- function(iIndex, A, AXsi, B, xsi, theta, 
           nnodes, maxK, recalc=TRUE)
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

	#*** subtract maximum in Rcpp to avoid numerical overflow
	rr0 <- Btheta + AXsi.tmp		
	rr1 <- tam_calc_prob_helper_subtract_max( rr0=rr0 )
	rr <- exp(rr1)
    rprobs <- rr / aperm( array( rep( colSums( aperm( rr , c(2,1,3) ) ,
					dims=1 , na.rm = TRUE) ,    maxK ), dim=dim(rr)[c(1,3,2)] ) , c(1,3,2) )	
	#---- output			
	res <- list("rprobs" = rprobs, "AXsi" = AXsi)
	return(res)
}
########################################################################  


calc_prob.v5 <- tam_mml_calc_prob
tam_calc_prob <- tam_mml_calc_prob
