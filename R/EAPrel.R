
#######################################################	
EAPrel <- function( theta , error , w = rep(1,length(theta) ))
{
	res <- WLErel_exclude_missings(theta=theta, error=error, w=w)
	theta <- res$theta
	error <- res$error
	w <- res$w
    v1 <- weighted_var( x = theta , w = w  )
    v2 <- weighted_mean( x = error^2 , w = w )	
    # v1 / (v1+v2) = 1 - v2 / ( v1 + v2 )
    rel <- v1 / ( v1 + v2 )
	return(rel)
}
#######################################################	
		