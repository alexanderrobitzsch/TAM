
tam_pv_mcmc_refresh_theta <- function( theta_acceptance_MH, adj_MH , adj_change_MH ,
		accrate_bound_MH , verbose , adj_min_MH = .1)
{
	theta_acceptance_MH$accrate <- theta_acceptance_MH$accepted / theta_acceptance_MH$n_samples	
	M_adj_MH <- mean(adj_MH)
	# tune adjustment rate
	accrate <- theta_acceptance_MH$accrate
	adj_MH <- ifelse( accrate < accrate_bound_MH[1] , adj_MH - adj_change_MH , adj_MH )
	adj_MH <- ifelse( accrate > accrate_bound_MH[2] , adj_MH + adj_change_MH , adj_MH )
	adj_MH <- ifelse( adj_MH < adj_min_MH , adj_min_MH , adj_MH )	
	if (verbose){
		v1 <- paste0("  Average acceptance rate = " , round(mean(accrate), 3) )
		v2 <- paste0("Average acceptance factor = " , round(M_adj_MH, 3) )
		cat(v1," | " , v2 , "\n")
		utils::flush.console()
	}
	theta_acceptance_MH <- 0*theta_acceptance_MH
	theta_acceptance_MH$adj_MH <- adj_MH
	#--- OUTPUT
	res <- list( adj_MH=adj_MH, theta_acceptance_MH=theta_acceptance_MH)
	return(res)	
}
