
tam_mml_create_nodes <- function(snodes, nodes, ndim, QMC,
		skillspace="normal", theta.k=NULL)
{
	thetasamp.density <- NULL
	theta2 <- NULL
	thetawidth <- NULL
	theta0.samp <- NULL
	theta <- NULL	
	
	#--- 
	if ( is.null(theta.k) & ( skillspace == "discrete") ){
		snodes <- 0
	}	
	
	#----------------------------------------
    #--- numeric integration
    if ( snodes == 0 ){ 
		theta <- as.matrix( expand.grid( as.data.frame( matrix( rep(nodes, ndim) , ncol = ndim ) ) ) )
		if ( ( skillspace != "normal") & ( ! is.null(theta.k) ) ){	  
			theta <- as.matrix( theta.k )
			nnodes <- nrow(theta)
		}		
		#we need this to compute sumsig2 for the variance
		theta2 <- tam_theta_sq(theta=theta, is_matrix = TRUE )
		# grid width for calculating the deviance
		thetawidth <- diff(theta[,1] )
		thetawidth <- ( ( thetawidth[ thetawidth > 0 ])[1] )^ndim 
		nnodes <- nrow(theta)		
    } 
	#----------------------------------------
    #--- stochastic integration
    if ( snodes > 0 ){ 
		# sampled theta values
		if (QMC){
			fac <- 1
			r1 <- sfsmisc::QUnif(n=snodes, min = 0, max = 1, n.min = 1, p=ndim, leap = 409)
			theta0.samp <- fac * stats::qnorm(r1)
			if (ndim==1){
				theta0.samp <- theta0.samp[ order(theta0.samp[,1]) , ]
			}
		} else {
			theta0.samp <- matrix( mvtnorm::rmvnorm( snodes , mean = rep(0,ndim) , 
                                        sigma = diag(1,ndim ) )	, nrow= snodes , ncol=ndim )			
		}
		nnodes <- nrow(theta0.samp)
    }
	#---- OUTPUT	
	res <- list( theta=theta, theta2=theta2, thetawidth=thetawidth,
					theta0.samp=theta0.samp, thetasamp.density=thetasamp.density,
					nodes=nodes, snodes=snodes, QMC=QMC, nnodes=nnodes,
					theta.k=theta.k)
	return(res)	
}