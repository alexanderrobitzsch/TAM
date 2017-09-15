## File Name: tam_pv_draw_pv_nonparametric.R
## File Version: 0.01
## File Last Change: 2017-05-25 19:55:10

tam_pv_draw_pv_nonparametric <- function(nstud, hwt1, theta, pv, ndim, pp)
{
	rn1 <- stats::runif( nstud )
	ind <- tam_interval_index( hwt1 , rn1 ) - 1				
	if (ndim==1){
		if (is.matrix(theta)){
			theta <- theta[,1]
		}
		pv[,pp] <- theta1 <- theta[ind]	
	} else {
		theta1 <- pv[ , (pp-1)*(ndim) + 1:ndim ] <- theta[ ind , ]
	}
	#--- OUTPUT
	res <- list(pv=pv, theta1=theta1)
	return(res)
}
	
