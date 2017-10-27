## File Name: tam_pv_mcmc_likelihood.R
## File Version: 0.11

tam_pv_mcmc_likelihood <- function( probs, resp, resp.ind, nstud, nitems , maxK )
{
	probs00 <- matrix( probs , nrow=nstud , ncol=nitems*maxK)	
	loglike <- tam_pv_mcmc_likelihood_Rcpp( probs=probs00, resp=resp, resp_ind=resp.ind, 
					maxK=maxK, nstud=nstud, nitems=nitems )
	return(loglike)
}
