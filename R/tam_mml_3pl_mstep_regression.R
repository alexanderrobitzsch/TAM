###############################################
# mstep.regression
tam_mml_3pl_mstep_regression <- function( resp , hwt ,  resp.ind , 
		pweights , pweightsM , Y , theta , theta2 , YYinv , ndim , 
		nstud , beta.fixed , variance , Variance.fixed , group , G , 
		snodes = 0 , thetasamp.density=NULL , nomiss=FALSE , group_indices = NULL,
		min.variance = 1E-5 , userfct.variance=NULL , variance_acceleration=NULL , 
		ridge_eps=1E-10 ,  beta , iter )
{
	# calculate item weights
	variance.fixed <- Variance.fixed
#  a0 <- Sys.time()	
	
	beta0 <- beta
	variance0 <- variance

	#*****
	# numerical integration	
	if ( snodes == 0){	
		# hwt ... N x q matrix		
		if (!nomiss){
			itemwt <- crossprod( hwt , resp.ind * pweightsM  )			
		}
		if ( nomiss ){
			itemwt <- matrix( colSums(hwt*pweights) , nrow=ncol(hwt) , ncol=ncol(resp.ind) )
		}						 			
 		# original implementation without missings
		thetabar <- hwt %*% theta
		sumbeta <- crossprod( Y , thetabar*pweights )
        sumsig2 <- as.vector( crossprod( colSums( pweights * hwt ) , theta2 ) )		
	}

	#****
	# Monte Carlo integration
	if ( snodes > 0 ){
		hwtS <- hwt
		# hwtS <- hwtS / rowSums( hwtS ) 
		hwtS <- tam_normalize_matrix_rows(hwtS)
		itemwt <- crossprod( hwtS , resp.ind * pweightsM  )
# cat("- calc itemwt ") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1				
		thetabar <- hwtS %*% theta
		sumbeta <- crossprod( Y ,  thetabar*pweights )
		sumsig2 <- as.vector( crossprod( colSums( pweights * hwtS ) , theta2 ) )
# cat("- sum sig2 modified ") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1	
	}
	
	#------------------------------------------
	#--- SINGLE GROUP
	#------------------------------------------

	# calculation of variance and regression coefficients					
	beta <- YYinv%*%sumbeta                     #new beta
	sumsig2 <- matrix(sumsig2,ndim,ndim)
	if (G==1){
		variance <- (sumsig2- crossprod( sumbeta , beta ) )/nstud  #new variance
	}
		
	# fixed beta coefficients
	if ( ! is.null( beta.fixed )){ 
		beta[ beta.fixed[,1:2,drop=FALSE] ] <- beta.fixed[,3] 
		beta <- as.matrix( beta , ncol=ndim )	
	}
	if (G==1){
		# fixed covariance matrix entries
		if ( ! is.null(variance.fixed) ){ 
			variance[ variance.fixed[,1:2,drop=FALSE] ] <- variance.fixed[,3]  	
			variance[ variance.fixed[,c(2,1),drop=FALSE] ] <- variance.fixed[,3]  		
		}						
		# prevents negative variance
		if( ndim == 1 ){  
			variance[ variance < min.variance ] <- min.variance 
		}	
		# ridge variance
		diag(variance) <- diag(variance) + ridge_eps
	}
	
	#------------------------------------------
	#--- MULTIPLE GROUPS	
	#------------------------------------------
	if ( G > 1){	
		# if ( snodes > 0 ){ 
		# 	hwt <- hwt / snodes 
		# 	hwt <- hwt / rowSums( hwt )				
		# }
		for (gg in 1:G){
			#gg <- 1 
			# ind.gg <- which( group == gg )
			ind.gg <- group_indices[[gg]]
			thetabar <- hwt[ind.gg,]%*%theta
			sumbeta <- crossprod( Y[ind.gg,] ,  thetabar*pweights[ind.gg] )
			sumsig2 <- colSums( ( pweights[ind.gg]*hwt[ind.gg,]) %*% theta2)   
			sumsig2 <- matrix(sumsig2,ndim,ndim)		
			v1 <- (sumsig2 - crossprod( sumbeta , beta) )/sum(pweights[ind.gg])
			variance[gg,1:ndim,1:ndim] <- v1
			if ( ! is.null(variance.fixed) ){ 
				var_fixed_gg <- variance.fixed[ variance.fixed[,1]  == gg , , drop=FALSE ]
				if ( nrow(var_fixed_gg) > 0 ){
					variance[ var_fixed_gg[,c(1,2,3),drop=FALSE] ] <- var_fixed_gg[,4]  	
					variance[ var_fixed_gg[,c(1,3,2),drop=FALSE] ] <- var_fixed_gg[,4]  		
				}
			}
		}	
	}		# end multiple groups	
	#-------------------------------------------------------------------
	
	#---- user function variance
	if ( ! is.null( userfct.variance ) ){  
		variance <- do.call( userfct.variance , list(variance=variance) )			
	}		
	
	#--- prevent small variance values
	if (G == 1){
		if( ndim == 1 ){  # prevent negative variance
			variance[ variance < min.variance ] <- min.variance 
		}			
		diag(variance) <- diag(variance) + 1E-10
	}				
	
	#--- variance acceleration
	if (G==1){
		if ( ! is.null( variance_acceleration) ){
			if ( variance_acceleration$acceleration != "none" ){		
				variance_acceleration <- tam_accelerate_parameters( xsi_acceleration=variance_acceleration , 
								xsi=as.vector(variance) , iter=iter , itermin=3)
				variance <- matrix( variance_acceleration$parm , nrow= nrow(variance) , ncol=ncol(variance) )
			}
		}
	}	
	
	#--- parameter change
	beta_change <- tam_parameter_change( beta , beta0 )
	variance_change <- tam_parameter_change( variance , variance0 )
	
	#---- output
    res <- list( "beta" = beta , "variance" = variance , "itemwt" = itemwt ,
				variance_acceleration=variance_acceleration, beta_change=beta_change,
				variance_change=variance_change)
	return(res)
}
############################################################################
