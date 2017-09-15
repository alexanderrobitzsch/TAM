## File Name: tam_pv_mcmc_inits_MH_sampling_objects.R
## File Version: 0.02
## File Last Change: 2017-08-14 09:32:48

tam_pv_mcmc_inits_MH_sampling_objects <- function( adj_MH, nstud)
{
	if ( length(adj_MH) == 1){	
		adj_MH <- rep( adj_MH, nstud)
	}
	theta_acceptance_MH <- as.data.frame( matrix( 0 , nrow=nstud, ncol=4) )
	colnames(theta_acceptance_MH) <- c("n_samples" , "accepted", "adj_MH", "accrate")
	theta_acceptance_MH$adj_MH <- adj_MH
	theta_acceptance_MH$adj_MH <- adj_MH
	#--- OUTPUT
	res <- list( adj_MH=adj_MH, theta_acceptance_MH=theta_acceptance_MH )
	return(res)
}
