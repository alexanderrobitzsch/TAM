
#####################################################
# computes reliability for one-dimensional WLEs
WLErel <- function( theta , error , w = rep(1,length(theta) )){
	res <- WLErel_exclude_missings(theta=theta, error=error, w=w)
	theta <- res$theta
	error <- res$error
	w <- res$w
    v1 <- weighted_var( x = theta , w = w  )
    v2 <- weighted_mean( x = error^2 , w = w  )
    # WLE_Rel = ( v1 - v2 ) / v1 = 1 - v2 / v1
    rel <- 1 - v2 / v1
	return(rel)
}
		
