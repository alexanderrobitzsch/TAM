## File Name: tam_pv_mcmc_compute_marginal_likelihood.R
## File Version: 0.02

tam_pv_mcmc_compute_marginal_likelihood <- function(pv, AXsi, B, guess, resp,
		resp.ind, maxK)
{
	nstud <- nrow(pv)
	nplausible <- attr(pv, "nplausible")
	D <- attr(pv, "D")
	like <- matrix( NA , nrow=nstud, ncol=nplausible)
	for (pp in 1:nplausible){
		theta <- pv[ , (pp-1)*D + 1:D + 1 ]
		like[,pp] <- tam_pv_mcmc_evaluate_likelihood( theta=theta, AXsi=AXsi, B=B, guess=guess, 
				resp=resp, resp.ind=resp.ind, maxK=maxK ) 	
	}
	like <- rowMeans(like)
	return(like)
}	
