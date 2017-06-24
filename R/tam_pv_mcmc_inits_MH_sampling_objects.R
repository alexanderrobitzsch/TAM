
tam_pv_mcmc_inits_MH_sampling_objects <- function( adj_MH, nstud)
{
	adj_MH <- rep( adj_MH, nstud)
	theta_acceptance_MH <- as.data.frame( matrix( 0 , nrow=nstud, ncol=4) )
	colnames(theta_acceptance_MH) <- c("n_samples" , "accepted", "adj_MH", "accrate")
	theta_acceptance_MH$adj_MH <- adj_MH
	theta_acceptance_MH$adj_MH <- adj_MH
	#--- OUTPUT
	res <- list( adj_MH=adj_MH, theta_acceptance_MH=theta_acceptance_MH )
	return(res)
}