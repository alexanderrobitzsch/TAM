## File Name: tam_pv_mcmc_postproc_theta_acceptance_MH.R
## File Version: 0.01
## File Last Change: 2017-05-29 11:55:22

tam_pv_mcmc_postproc_theta_acceptance_MH <- function(theta_acceptance_MH)
{
	theta_acceptance_MH$accrate <- theta_acceptance_MH$accepted / theta_acceptance_MH$n_samples	
	attr(theta_acceptance_MH, "M_accrate") <- mean( theta_acceptance_MH$accrate )
	attr(theta_acceptance_MH, "M_adj_MH") <- mean( theta_acceptance_MH$adj_MH )
	return(theta_acceptance_MH)
}
