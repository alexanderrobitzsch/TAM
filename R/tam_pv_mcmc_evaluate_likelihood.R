## File Name: tam_pv_mcmc_evaluate_likelihood.R
## File Version: 0.02
## File Last Change: 2017-05-30 11:57:54

tam_pv_mcmc_evaluate_likelihood <- function(theta, 
	AXsi, B, guess, resp, resp.ind, maxK)
{
	nitems <- ncol(resp)
	nstud <- nrow(resp)
	#* compute response probabilities and likelihood
	probs <- tam_irf_3pl(theta=theta, AXsi=AXsi, B=B, guess=guess)		
	like <- tam_pv_mcmc_likelihood( probs=probs, resp=resp, resp.ind=resp.ind, 
							nstud=nstud, nitems=nitems, maxK=maxK )
	#--- OUTPUT
	return(like)
}
