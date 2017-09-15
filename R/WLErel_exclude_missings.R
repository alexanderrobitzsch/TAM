## File Name: WLErel_exclude_missings.R
## File Version: 0.01
## File Last Change: 2017-06-01 13:41:28

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
