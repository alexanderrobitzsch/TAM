
tam_pv_mcmc_parameter_samples <- function(beta_samples, variance_samples)
{	
	#--- parameter_samples
	parameter_samples <- data.frame( beta_samples, variance_samples )
	saved_iter <- attr( beta_samples, "saved_iter") 	
	parameter_samples <- coda::mcmc( data = parameter_samples, start = min(saved_iter),
							end = max(saved_iter) , thin = 1)
	
	#--- beta estimates
	beta_groups <- attr( beta_samples, "beta_groups")
	D <- attr( beta_samples, "D")
	G <- attr( beta_samples, "G")
	ncol_Y <- attr( beta_samples, "ncol_Y")
	beta_index <- attr( beta_samples, "beta_index")	
	beta_M <- colMeans(beta_samples)
	beta <- list()	
	if (beta_groups){
		for (gg in 1:G){
			ind_gg <- beta_index[[gg]]
			beta[[gg]] <- matrix( beta_M[ ind_gg ] , nrow=ncol_Y, ncol=D )
		}	
	}
	if ( ! beta_groups ){
		beta0 <- matrix( beta_M , nrow=ncol_Y, ncol=D )
		for (gg in 1:G){
			beta[[gg]] <- beta0
		}	
	}
	
	#--- variance estimates
	variance <- list()
	variance_index <- attr( variance_samples, "variance_index")
	variance_M <- colMeans(variance_samples)
	for (gg in 1:G){
		ind_gg <- variance_index[[gg]]
		variance[[gg]] <- tam_vec2symmmatrix(variance=variance_M[ind_gg] )
	}		
		
	#--- OUTPUT
	res <- list(parameter_samples=parameter_samples, beta=beta, variance=variance)
	return(res)
}
	