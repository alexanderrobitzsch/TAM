## File Name: tam_pv_mcmc_update_plausible_values.R
## File Version: 0.01
## File Last Change: 2017-05-28 15:51:45


tam_pv_mcmc_update_plausible_values <- function(pv, theta, pv_index_matrix)
{
	index <- attr(pv,"last_pv") <- attr(pv,"last_pv")  + 1
	pv[ , pv_index_matrix[[index]] ] <- theta
	return(pv)
}
