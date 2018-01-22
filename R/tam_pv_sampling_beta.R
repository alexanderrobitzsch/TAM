## File Name: tam_pv_sampling_beta.R
## File Version: 0.08

tam_pv_sampling_beta <- function( theta1, ndim , Y , pweights, eps=1E-8 )
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
	if ( sum( is.na(beta) ) >= 0 ){
		C <- as.matrix(Y1)
		C1 <- crossprod(C)
		diag(C1) <- diag(C1)*(1 + eps)
		D <- crossprod(C, theta1 )
		beta <- solve(C1) %*% D
	}
	if ( ndim == 1 ){
		beta <- matrix( beta , ncol=1 )
	}	
	return(beta)
}
