
WLErel_exclude_missings <- function(theta, error, w)
{
	ind <- which( ! is.na(theta) )
	theta <- theta[ind]
	error <- error[ind]
	w <- w[ind]
	#--- OUTPUT
	res <- list( theta=theta, error=error, w=w)
	return(res)
}	