
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