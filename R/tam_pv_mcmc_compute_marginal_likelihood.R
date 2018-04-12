## File Name: tam_pv_mcmc_compute_marginal_likelihood.R
## File Version: 0.07

tam_pv_mcmc_compute_marginal_likelihood <- function(pv, AXsi, B, guess, resp,
		resp.ind, maxK, resp_ind_bool)
{
	nstud <- nrow(pv)
	nplausible <- attr(pv, "nplausible")
	D <- attr(pv, "D")
	like <- matrix( NA , nrow=nstud, ncol=nplausible)
	for (pp in 1:nplausible){
		theta <- pv[ , (pp-1)*D + 1:D + 1 ]
		like[,pp] <- tam_pv_mcmc_evaluate_likelihood( theta=theta, AXsi=AXsi, B=B, guess=guess, 
				resp=resp, resp.ind=resp.ind, maxK=maxK, resp_ind_bool=resp_ind_bool ) 	
	}
	like <- rowMeans(like)
	return(like)
}	
