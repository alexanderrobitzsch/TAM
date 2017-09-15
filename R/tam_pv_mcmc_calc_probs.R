## File Name: tam_pv_mcmc_calc_probs.R
## File Version: 0.05
## File Last Change: 2017-08-17 17:19:45

tam_pv_mcmc_calc_probs <- function(theta_new, AXsi, B, guess, subtract_max=FALSE,
		resp.ind)
{
	res <- tam_pv_mcmc_calc_probs_irf_3pl(theta=theta_new, AXsi, B, guess, subtract_max=FALSE,
				resp.ind=resp.ind)	
	return(res)
}
