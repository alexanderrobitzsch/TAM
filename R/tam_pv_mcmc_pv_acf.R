## File Name: tam_pv_mcmc_pv_acf.R
## File Version: 0.03


tam_pv_mcmc_pv_acf <- function(pv, theta_samples_mean, theta_samples_sd)
{
	nplausible <- attr(pv, "nplausible")
	D <- attr(pv, "D")	
	theta_acf <- rep(NA,D)
	names(theta_acf) <- paste0("Dim",1:D)
	for (dd in 1:D){
		pv_dd <- pv[ , seq( dd + 1, D*nplausible +1 , D ) ]
		pv_dd <- ( pv_dd - theta_samples_mean[,dd] ) / theta_samples_sd[,dd]
		res <- tam_acf_matrix(x=pv_dd)
		theta_acf[dd] <- mean(res)
	}
	return(theta_acf)
}
