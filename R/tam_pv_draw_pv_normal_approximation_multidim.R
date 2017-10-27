## File Name: tam_pv_draw_pv_normal_approximation_multidim.R
## File Version: 0.02

tam_pv_draw_pv_normal_approximation_multidim <- function(theta, hwt, pp, ndim, pv)
{
	theta_samp <- tam_pv_sample_theta_multidim( post=hwt, theta=theta)
	pv[ , (pp-1)*(ndim) + 1:ndim ] <- theta1 <- theta_samp
	#--- OUTPUT
	res <- list(pv=pv, theta1=theta1)
	return(res)
}
