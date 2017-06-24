
tam_pv_mcmc_postproc_theta_posterior <- function(theta_samples_mean,
		theta_samples_sd, N_samples, pweights)
{
	D <- ncol(theta_samples_mean)
	theta_samples_mean <- theta_samples_mean / N_samples
	eps <- 1E-10
	theta_samples_sd <- sqrt( ( theta_samples_sd - N_samples * theta_samples_mean^2 + eps ) / 
								( N_samples - 1) )
	EAP_rel <- rep(NA,D)
	names(EAP_rel) <- paste0("Dim",1:D)
	for (dd in 1:D){
		EAP_rel[dd] <- EAPrel(theta=theta_samples_mean[,dd] , 
							error=theta_samples_sd[,dd], w = pweights )
	}
	
	#--- OUTPUT
	res <- list( theta_samples_mean=theta_samples_mean, 
				theta_samples_sd=theta_samples_sd, EAP_rel=EAP_rel)
	return(res)
}	