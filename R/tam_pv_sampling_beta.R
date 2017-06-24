
tam_pv_sampling_beta <- function( theta1, ndim , Y , pweights )
{
	# bootstrap sample of persons to get sampled beta coefficients
	if ( ndim > 1 ){
		N <- nrow(theta1)			
		ind <- sample( 1:N , N , replace=TRUE)
		theta1 <- theta1[ ind , ]				
		Y1 <- Y[ ind , , drop=FALSE ]
	} else {
		N <- length(theta1)
		ind <- sample( 1:N , N , replace=TRUE)
		theta1 <- theta1[ ind ]				
		Y1 <- Y[ ind , , drop=FALSE ]				
	}
	#-- fit linear model
	modlm <- stats::lm( theta1 ~ -1 + as.matrix(Y1) , weights = pweights[ind])
	beta <- modlm$coef			# sampled regression coefficients
	if ( ndim == 1 ){
		beta <- matrix( beta , ncol=1 )
	}	
	return(beta)
}