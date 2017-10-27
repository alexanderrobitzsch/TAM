## File Name: tam_pv_mcmc_postproc_ic.R
## File Version: 0.05

tam_pv_mcmc_postproc_ic <- function(parameter_samples, deviance_samples,
		theta_samples_mean, AXsi, B, guess, beta, variance, group_index, G, Y,
		resp, resp.ind, maxK, pv )
{
	
	nstud <- nrow(Y)
	nitems <- ncol(resp)
	D <- attr( pv, "D")
	nplausible <- attr( pv, "nplausible")
	
	#--- init ic vector
	ic <- c()
	
	#*******************
	# inference based on marginal likelihood
	like <- tam_pv_mcmc_compute_marginal_likelihood( pv=pv, AXsi=AXsi, B=B, guess=guess, 
				resp=resp, resp.ind=resp.ind, maxK=maxK ) 
	ic$deviance <- -2*sum( log( like ) )
	ic$n <- nstud
	ic$Npars <- ic$np <- ncol(parameter_samples)
	#-- compute all criteria
	ic <- tam_mml_ic_criteria(ic=ic)
	
	#*****************
	# fully Bayesian inference
	theta <- theta_samples_mean
	
	#--- Dbar
	ic$Dbar <- mean(deviance_samples)
	#--- Dhat
	like <- tam_pv_mcmc_evaluate_likelihood( theta=theta, AXsi=AXsi, B=B, guess=guess, 
				resp=resp, resp.ind=resp.ind, maxK=maxK ) 
	ic$Dhat <- -2*sum( log(like) )
	
	#--- pD
	ic$pD <- ic$Dbar - ic$Dhat
	
	#--- DIC
	ic$DIC <- ic$Dhat + 2 * ic$pD

	#--- OUTPUT
	return(ic)
}
