## File Name: tam_mml_mstep_regression.R
## File Version: 9.39
###############################################
# mstep.regression
tam_mml_mstep_regression <- function( resp , hwt ,  resp.ind , 
	pweights , pweightsM , Y , theta , theta2 , YYinv , ndim , 
	nstud , beta.fixed , variance , Variance.fixed , group , G , 
	snodes = 0 , thetasamp.density=NULL , nomiss=FALSE, iter= 1E9,
	min.variance = 0, userfct.variance = NULL ,
	variance_acceleration = NULL, est.variance=TRUE, beta = NULL,
	latreg_use = FALSE, gwt=NULL, importance_sampling=FALSE )
{
	variance.fixed <- Variance.fixed
	beta_old <- beta
	variance_old <- variance
	itemwt <- NULL
#  a0 <- Sys.time()	
	#----- numerical integration	
	if ( snodes == 0){
		if (!latreg_use){
			if ( ! nomiss ){
				itemwt <- crossprod( hwt , resp.ind * pweightsM  )			
			}
			if ( nomiss ){
				itemwt <- matrix( colSums(hwt*pweights) , nrow=ncol(hwt) , ncol=ncol(resp.ind) )
			}						 			
		}
		thetabar <- hwt %*% theta
		sumbeta <- crossprod( Y , thetabar*pweights )
        sumsig2 <- as.vector( crossprod( colSums( pweights * hwt ) , theta2 ) )		
	}
	#----- Monte Carlo integration	
	if ( snodes > 0 ){
		if (importance_sampling){
			hwt0 <- hwt / gwt	# likelihood
			TP <- length(thetasamp.density)
			tsd <- tam_matrix2( thetasamp.density , nrow=nstud , ncol=TP)
			rej_prob <- gwt / tsd
			rnm <- tam_matrix2( stats::runif(TP) , nrow=nstud , ncol=TP )
			hwt_acc <- 1 * ( rej_prob > rnm	)
			# hwt <- tam_normalize_matrix_rows( hwt0 * hwt_acc )
			hwt <- tam_normalize_matrix_rows( hwt0 * tsd * hwt_acc )
		}
	
		if ( ! latreg_use){
			hwt <- hwt / rowSums( hwt )   # maybe this can be fastened
			itemwt <- crossprod( hwt , resp.ind*pweightsM  )
		}
		thetabar <- hwt %*% theta
		sumbeta <- crossprod( Y,  thetabar*pweights )
		sumsig2 <- as.vector( crossprod( colSums( pweights * hwt ) , theta2 ) )
	}					
	#----------------------------------------
	# calculation of variance and regression coefficients					
    beta <- YYinv %*% sumbeta     # updated beta
    sumsig2 <- matrix(sumsig2, nrow=ndim, ncol=ndim)
    if (G==1){ 
		variance <- (sumsig2- crossprod( sumbeta , beta ) )/nstud  #new variance
	}
	#----------------------------------------			
	# fixed beta coefficients
	if ( ! is.null( beta.fixed )){ 
		beta[ beta.fixed[,1:2,drop=FALSE] ] <- beta.fixed[,3] 
		beta <- as.matrix( beta , ncol=ndim )	
	}
	#----------------------------------------
	# fixed covariance matrix entries
	if ( ! is.null(variance.fixed) ){ 
		variance[ variance.fixed[,1:2,drop=FALSE] ] <- variance.fixed[,3]  	
		variance[ variance.fixed[,c(2,1),drop=FALSE] ] <- variance.fixed[,3]  		
	}				
	#----------------------------------------

	#****************************************
	###### Multiple Groups ##############
	if ( G > 1){	# begin multiple groups
		if ( snodes > 0 ){ 
				hwt <- hwt / snodes 
				hwt <- hwt / rowSums( hwt )				
		}
		for (gg in 1:G){
			ind.gg <- which( group == gg )
			thetabar <- hwt[ind.gg,] %*% theta
			sumbeta <- crossprod( Y[ind.gg,],  thetabar*pweights[ind.gg] )
			sumsig2 <- colSums((pweights[ind.gg]*hwt[ind.gg,]) %*% theta2)   
			sumsig2 <- matrix(sumsig2,ndim,ndim)
			variance[ind.gg] <- (sumsig2- crossprod(sumbeta, beta) )/sum(pweights[ind.gg])
		}
	}		# end multiple groups			
	#*************************************************
	
	#--- adjustments of variance estimation
	eps <- 1E-10
	if( ndim == 1 ){  # prevent negative variance
		variance[ variance < min.variance ] <- min.variance 
    }
    if (G == 1){
        diag(variance) <- diag(variance) + eps
    }
    if ( ! est.variance ){ 
		if ( G == 1 ){ 
				variance <- stats::cov2cor(variance)  
		} # fix variance at 1  
		if ( G > 1 ){ 
			variance[ group == 1 ] <- 1 
		}     
		# fix variance of first group to 1
	}
	
	#--- function for constraining the variance	  
	if ( ! is.null( userfct.variance ) ){  
		variance <- do.call( userfct.variance , list(variance) )			
	}

	if ( ( iter < 4 ) ){	
		na_variance <- sum(is.na(variance)) > 0
		if (na_variance){
			v1 <- paste0("Problems in variance estimation.\n ",
					"Try to Choose argument control=list( xsi.start0=TRUE, ...) ")
			stop(v1)
		}
	}	
	
	#--- variance acceleration
	if ( ! is.null(variance_acceleration) ){
		if ( variance_acceleration$acceleration != "none" ){		
			variance_acceleration <- tam_accelerate_parameters( xsi_acceleration=variance_acceleration , 
						xsi=as.vector(variance) , iter=iter , itermin=3)
			variance <- matrix( variance_acceleration$parm , nrow= nrow(variance) , ncol=ncol(variance) )
		}
	}
		
	#---- parameter change
	beta_change <- max( abs( beta - beta_old ) )	
	variance_change <- max( abs( as.vector( variance ) - as.vector( variance_old ) ) )
	#----- OUTPUT
    res <- list( beta = beta , variance = variance , itemwt = itemwt ,
				variance_acceleration=variance_acceleration, beta_change=beta_change,
				variance_change=variance_change )
	return(res)
}
#-----------------------------------------------------

mstep.regression <- tam_mml_mstep_regression



