
###################################################
# covariance stabilization in TAM
###################################################
tam_ginv <- function(x, eps=.05)
{
	svd_var <- svd(x)
	d0 <- d <- svd_var$d		
	ind <- which( d < eps)
	variance <- x
	if (length(ind)>0){
		d[ ind ] <- eps
		d <- d / sum(d) * sum(d0)
		variance <- svd_var$u %*% diag(d) %*% t( svd_var$v  )
	}
	return(variance)
}