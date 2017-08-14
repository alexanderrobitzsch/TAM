
tam_pv_mcmc_proposal_theta <- function(theta, nstud, variance, adj_MH, D, G, group_index)
{
	nstud <- nrow(theta)
	theta_new <- matrix( NA , nrow=nstud , ncol=D)
	for (gg in 1:G){
		ind_gg <- group_index[[gg]]
		nstud_gg <- attr( group_index , "N_groups")[gg]	
		variance_gg <- variance[[gg]]
		mean_gg <- rep(0, D )
		samp_values <- matrix( CDM::CDM_rmvnorm( nstud_gg , mean=mean_gg, sigma=variance_gg ) , ncol=D )
		theta_new[ind_gg,] <- theta[ind_gg,] + adj_MH[ind_gg] * samp_values
	}
	return(theta_new)
}