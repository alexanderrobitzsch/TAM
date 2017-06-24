
tam_pv_mcmc_prior_density <- function( theta, beta, variance, Y, log=FALSE,
		G, group_index, beta_groups )
{
	dens_theta <- rep(NA, nrow(theta) )
	D <- ncol(variance[[1]])	
	NB <- ncol(Y)
	nstud <- nrow(Y)
	theta_exp <- matrix( NA , nrow=nstud, ncol=D )			
	for (gg in 1:G){
		ind_gg <- group_index[[gg]]		
		Y_gg <- Y[ ind_gg ,, drop=FALSE]
		theta_gg <- theta[ ind_gg , , drop=FALSE ]  
		if (beta_groups){
			beta_gg <- beta[[gg]]
		} else {
			beta_gg <- beta
		}		
		theta_exp_gg <- matrix( Y_gg %*% beta_gg , ncol=D)		
		dens_theta[ind_gg] <- tam_dmvnorm( x= theta_gg, mean=theta_exp_gg ,	
								sigma=variance[[gg]] , log = log )
	}
	return(dens_theta)
}