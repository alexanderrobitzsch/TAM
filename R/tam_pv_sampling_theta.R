
tam_pv_sampling_theta <- function( theta.model, ndim, normal.approx , tamobj, MEAP,
		SDEAP, np.adj, theta, ntheta , mu1 , Sigma1, na.grid=5 )
{
	if ( ! theta.model ){
       # 1-dimensional PV imputation	   
		if (ndim == 1){
			# unidimensional theta simulation
			if ( ! normal.approx){			
				theta <- matrix( stats::rnorm( ntheta , mean = MEAP , sd = np.adj*SDEAP )  , ncol= 1)	
				theta <- theta[ order( theta[,1] ) , , drop=FALSE]
			} else {
				theta <- matrix( SDEAP * seq( - na.grid, na.grid , len=ntheta ) + MEAP , ncol=1 )		 
			}
		}
		#*****************************
		# multidimensional PV imputation									
		if( ndim > 1 ){
			theta <- MASS::mvrnorm( ntheta , mu = mu1 , Sigma = Sigma1 )
		}
	}
	if ( theta.model ){
	      theta <- tamobj$theta 
	}
	#--- OUTPUT
	res <- list(theta=theta)
	return(res)
}
				