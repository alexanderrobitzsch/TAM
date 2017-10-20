## File Name: tam.pv.mcmc.R
## File Version: 0.813
## File Last Change: 2017-08-23 10:02:26

tam.pv.mcmc <- function( tamobj, Y=NULL , group=NULL, beta_groups = TRUE , 
				nplausible=10, level = .95, n.iter = 1000 ,
				n.burnin = 500, adj_MH = .5, adj_change_MH = .05 , 
				refresh_MH = 50, accrate_bound_MH = c(.45, .55), 
				sample_integers = FALSE, theta_init = NULL, print_iter = 20 , 
				verbose = TRUE)
{
    s1 <- Sys.time()
	CALL <- match.call()
	#--- extract values from TAM object
	res <- tam_pv_mcmc_proc_input( tamobj=tamobj, group=group, Y=Y )
	person <- res$person
	is_tam_class <- res$is_tam_class
	resp <- res$resp
	AXsi <- res$AXsi
	B <- res$B
	guess <- res$guess
	resp.ind <- res$resp.ind
	pweights <- res$pweights
	nitems <- res$nitems
	nstud <- res$nstud
	maxK <- res$maxK
	D <- res$D
	nstud <- res$nstud
	pid <- res$pid
	group <- res$group
	groups <- res$groups
	G <- res$G
	group_index <- res$group_index
	Y <- res$Y
	
	#--- compute initial person parameters
	theta <- theta0 <- tam_pv_mcmc_inits_theta(person=person, theta_init=theta_init)	
	
	#--- compute response probabilities theta value
	probs0 <- tam_irf_3pl(theta=theta, AXsi=AXsi, B=B, guess=guess)
	loglike <- tam_pv_mcmc_likelihood( probs=probs0, resp=resp, resp.ind=resp.ind, nstud=nstud, 
					nitems=nitems, maxK=maxK ) 
					
	#--- add colnames to Y if not provided
	Y <- tam_pv_mcmc_proc_regressors(Y=Y)
	
	#--- compute initial beta and variance parameters
	res <- tam_pv_mcmc_sample_beta_variance( theta=theta , Y=Y, nstud=nstud, 
					pweights=pweights, samp.regr=FALSE, G=G, group_index=group_index,
					beta_groups=beta_groups, sample_integers=sample_integers )
	beta <- res$beta
	variance <- res$variance
	
	#--- init adjustment factor for MH sampling
	res <- tam_pv_mcmc_inits_MH_sampling_objects( adj_MH=adj_MH, nstud=nstud ) 
	adj_MH <- res$adj_MH
	theta_acceptance_MH <- res$theta_acceptance_MH
	
	#--- create objects for drawing plausible values
	res <- tam_pv_mcmc_inits_plausible_values_objects( n.burnin=n.burnin, n.iter=n.iter, 
					nplausible=nplausible, D=D, nstud=nstud, pid=pid ) 
	pv <- res$pv
	pv_iter <- res$pv_iter
	pv_index_matrix <- res$pv_index_matrix	
	nplausible <- res$nplausible
	
	#--- create objects for saving sampled latent regression parameters
	res <- tam_pv_mcmc_inits_sampled_parameters_objects( n.burnin=n.burnin, n.iter=n.iter, 
				beta=beta, variance=variance, G=G, Y=Y, beta_groups=beta_groups ) 
	beta_samples <- res$beta_samples
	variance_samples <- res$variance_samples
	deviance_samples <- res$deviance_samples
	N_samples <- res$N_samples
	saved_iter <- res$saved_iter
	theta_samples_mean <- res$theta_samples_mean
	theta_samples_sd <- res$theta_samples_sd
	
	iterate_mcmc <- TRUE
	iter <- 1
	
	#--------------- BEGIN MCMC ITERATIONS ---------------------
	while (iterate_mcmc){

		#*** individual prior distributions
		log_dens_theta <- tam_pv_mcmc_prior_density( theta=theta, beta=beta, variance=variance, 
							Y=Y, log=TRUE,	G=G, group_index=group_index, 
							beta_groups=beta_groups)
		dens_theta <- exp( log_dens_theta )				
 		
		#--- new theta proposal and evaluation of prior density
		theta_new <- tam_pv_mcmc_proposal_theta( theta=theta, nstud=nstud, variance=variance, 
							adj_MH=adj_MH, D=D,	G=G, group_index=group_index ) 								
		log_dens_theta_new <- tam_pv_mcmc_prior_density( theta=theta_new, beta=beta, variance=variance, 
								Y=Y, log=TRUE,	G=G, group_index=group_index,
								beta_groups=beta_groups )
		dens_theta_new <- exp( log_dens_theta_new )							
		
		#--- compute response probabilities theta value
		probs_new <- tam_pv_mcmc_calc_probs( theta_new=theta_new, AXsi=AXsi, B=B, guess=guess, 
							subtract_max=FALSE, resp.ind=resp.ind ) 
		loglike_new <- tam_pv_mcmc_likelihood( probs=probs_new, resp=resp, resp.ind=resp.ind, 
							nstud=nstud, nitems=nitems, maxK=maxK ) 	
							
		#--- Metropolis Hastings Ratio and sampled theta values
		res <- tam_pv_mcmc_theta_MH_ratio_accept( loglike=loglike, log_dens_theta=log_dens_theta, 
					loglike_new=loglike_new, log_dens_theta_new=log_dens_theta_new, 
					theta_acceptance_MH=theta_acceptance_MH, theta=theta, theta_new=theta_new ) 
		loglike <- res$loglike
		theta <- res$theta
		deviance <- res$deviance
		theta_acceptance_MH <- res$theta_acceptance_MH
		
		#--- evaluate acceptance rate
		if ( ( iter <= n.burnin ) & ( iter %% refresh_MH == 0 ) ){
			res <- tam_pv_mcmc_refresh_theta( theta_acceptance_MH=theta_acceptance_MH, adj_MH=adj_MH, 
						adj_change_MH=adj_change_MH, accrate_bound_MH=accrate_bound_MH, 
						verbose=verbose )
			adj_MH <- res$adj_MH
			theta_acceptance_MH <- res$theta_acceptance_MH
		}
		
		#--- save plausible values
		if (iter %in% pv_iter ){
			pv <- tam_pv_mcmc_update_plausible_values(pv=pv, theta=theta, 
							pv_index_matrix=pv_index_matrix)
		}
		
		#--- sample new regression parameters
		res <- tam_pv_mcmc_sample_beta_variance( theta=theta , Y=Y, nstud=nstud, 
						pweights=pweights, samp.regr=TRUE,  G=G, group_index=group_index,
						beta_groups=beta_groups , sample_integers=sample_integers )
		beta <- res$beta
		variance <- res$variance
		
		#--- save sampled parameters
		if ( iter %in% saved_iter ){
			res <- tam_pv_mcmc_save_parameters( beta=beta, beta_samples=beta_samples, 
						variance=variance, variance_samples=variance_samples, deviance=deviance, 
						deviance_samples=deviance_samples, theta_samples_mean=theta_samples_mean,
						theta_samples_sd=theta_samples_sd, theta=theta) 
			beta_samples <- res$beta_samples
			variance_samples <- res$variance_samples
			deviance_samples <- res$deviance_samples
			theta_samples_mean <- res$theta_samples_mean
			theta_samples_sd <- res$theta_samples_sd	
		}
		#--- iteration index update
		iter <- iter + 1
		if (iter > n.iter){
			iterate_mcmc <- FALSE
		}
		#--- print progress
		if ( verbose ){
			if (iter %% print_iter == 0 ){
				cat("* Iteration ", iter , "\n")
			}
			utils::flush.console()
		}		
		
	}
	#--------------- END MCMC ITERATIONS ---------------------	
	
	#-- compute acceptance rate and update theta acceptanec object
	theta_acceptance_MH <- tam_pv_mcmc_postproc_theta_acceptance_MH(theta_acceptance_MH=theta_acceptance_MH)
	
	#--- transform samples in matrices to coda mcmc object
	res <- tam_pv_mcmc_parameter_samples( beta_samples=beta_samples, 
				variance_samples=variance_samples ) 
	parameter_samples <- res$parameter_samples
	beta <- res$beta
	variance <- res$variance
	correlation <- res$correlation
	
	#--- mean, SD and reliability of theta posterior distribution
	res <- tam_pv_mcmc_postproc_theta_posterior( theta_samples_mean=theta_samples_mean, 
				theta_samples_sd=theta_samples_sd, N_samples=N_samples, pweights=pweights )
	theta_samples_mean <- res$theta_samples_mean
	theta_samples_sd <- res$theta_samples_sd
	EAP_rel <- res$EAP_rel
	
	#--- evaluate information criteria
	ic <- tam_pv_mcmc_postproc_ic( parameter_samples=parameter_samples, deviance_samples=deviance_samples, 
				theta_samples_mean=theta_samples_mean, AXsi=AXsi, B=B, guess=guess, beta=beta, 
				variance=variance, group_index=group_index, G=G, Y=Y, resp=resp, 
				resp.ind=resp.ind, maxK=maxK, pv=pv ) 

	#--- autocorrelation function for plausible values
	theta_acf <- tam_pv_mcmc_pv_acf( pv=pv, theta_samples_mean=theta_samples_mean, 
					theta_samples_sd=theta_samples_sd ) 				
					
	#--- parameter summary
	parameter_summary <- tam_pv_mcmc_parameter_summary( parameter_samples=parameter_samples,
							level=level ) 
	
	s2 <- Sys.time()	
	#--- OUTPUT
	res <- list( pv=pv, group=group, groups=groups, G=G, parameter_samples=parameter_samples,
					ic=ic, theta_acf = theta_acf,
					deviance_samples=deviance_samples, 
					theta_acceptance_MH=theta_acceptance_MH,
					theta_samples_mean=theta_samples_mean, 
					theta_samples_sd=theta_samples_sd, theta_last = theta , 
					EAP_rel=EAP_rel, 
					beta=beta, variance=variance, correlation=correlation,
					parameter_summary=parameter_summary,
					nplausible=nplausible, ndim=D, pweights=pweights, pid=pid,
					n.iter=n.iter, n.burnin=n.burnin, ndim=D,
					nplausible=nplausible,
					time = c(s1,s2,s2-s1), CALL=CALL )
	class(res) <- "tam.pv.mcmc"
	return(res)
}
