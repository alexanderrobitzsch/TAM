## File Name: tam_pv_mcmc_likelihood_R.R
## File Version: 0.06

tam_pv_mcmc_likelihood_R <- function( probs, resp1, resp_ind_bool, nitems)
{
	nstud <- nrow(resp1)
	loglike <- rep(1,nstud)
	probs_index_ii <- cbind( 1:nstud , 0 )
	for (ii in 1:nitems){
		probs_ii <- probs[ , ii , ]
		probs_index_ii[,2] <- resp1[,ii]
		incr <- ifelse( resp_ind_bool[,ii] , probs_ii[ probs_index_ii ] , 1 )
		loglike <- loglike * incr
	}
	return(loglike)
}
