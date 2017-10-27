## File Name: tam_mml_3pl_variance_fixed.R
## File Version: 0.04


tam_mml_3pl_variance_fixed <- function( variance , variance.inits , 
	G , ndim , variance.fixed, est.variance)
{

	#***********************************
	# single group
	#***********************************
	if (G==1){
		if ( ! is.null( variance.inits ) ){
			variance <- variance.inits
		} else {
			variance <- diag( ndim ) 
		}
		
		if ( ! is.null(variance.fixed) ){
		  variance[ variance.fixed[,1:2 ,drop=FALSE] ] <- variance.fixed[,3]
		  variance[ variance.fixed[,c(2,1) ,drop=FALSE] ] <- variance.fixed[,3]	
		}
		if ( ! est.variance ){
			variance.fixed <- cbind( 1:ndim , 1:ndim , 1) 
		}
	}
	
	#***********************************
	# multiple groups
	#***********************************
	if (G>1){	
		if ( ! is.null( variance.inits ) ){
			variance <- variance.inits
		} else {
			variance <- array( 0 , dim=c(G,ndim,ndim) )
			for (gg in 1:G){
				variance[gg,,] <- diag(ndim)
			}
		}
			
		if ( ! is.null(variance.fixed) ){ 
			var_fixed_gg <- variance.fixed[ variance.fixed[,1]  == gg , , drop=FALSE ]
			if ( nrow(var_fixed_gg) > 0 ){
				variance[ var_fixed_gg[,c(1,2,3),drop=FALSE] ] <- var_fixed_gg[,4]  	
				variance[ var_fixed_gg[,c(1,3,2),drop=FALSE] ] <- var_fixed_gg[,4]  		
			}
		}								
		if ( ! est.variance ){
			variance.fixed <- cbind( 1 , 1:ndim , 1:ndim , 1) 
		}					
	}
	#---- output
	res <- list(variance = variance , variance.fixed = variance.fixed)		
	return(res)
}
