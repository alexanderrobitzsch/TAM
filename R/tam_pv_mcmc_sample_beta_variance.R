
tam_pv_mcmc_sample_beta_variance <- function( theta , Y, nstud , pweights, samp.regr=FALSE,
		G, group_index, beta_groups)
{	
	formula_theta <- theta0 ~ 0 + Y0	
	D <- ncol(theta)
	group_index0 <- group_index
	N_groups_cumsum <- attr(group_index,"N_groups_cumsum")

	ind <- seq(1, nrow(theta) )
	if (beta_groups){
		beta <- list()
	}
	#--- bootstrap draw
	if (samp.regr){
		# ind <- sample( 1:nstud , replace=TRUE )
		ind <- c()
		for (gg in 1:G){
			N_group_gg <- attr(group_index0, "N_groups")[gg] 
			group_index[[gg]] <- sample( group_index0[[gg]] , N_group_gg, replace=TRUE )
		}
		ind <- unlist( group_index )						
	}		
	#--- estimate linear regression
	if ( ! beta_groups ){
		Y0 <- as.matrix(Y[ ind , , drop=FALSE])
		theta0 <- theta[ ind, , drop=FALSE ]
		pweights0 <- pweights[ind]		
		res <- tam_pv_mcmc_sample_beta_variance_lm_beta(Y0=Y0, theta0=theta0, 
							pweights0=pweights0, D=D, use_lm=FALSE)
		beta2 <- res$beta2
		res2 <- res$res2												
		beta <- beta2		
	}
	variance <- list()
	for (gg in 1:G){	
		ind_gg <- group_index[[gg]]
		if (beta_groups){	
			Y0 <- as.matrix( Y[ ind_gg , , drop=FALSE] )
			theta0 <- theta[ ind_gg, , drop=FALSE ]
			pweights0 <- pweights[ind_gg]
			res <- tam_pv_mcmc_sample_beta_variance_lm_beta(Y0=Y0, theta0=theta0, 
							pweights0=pweights0, D=D, use_lm=FALSE)
			beta2 <- res$beta2
			res2 <- res$res2								
			beta[[gg]] <- beta2	
			res_gg <- res2
			wgt_gg <- pweights0
		}
		if ( ! beta_groups ){
			if ( samp.regr ){
				ind_gg <- seq( N_groups_cumsum$start[gg] , N_groups_cumsum$end[gg] )
			} else {
				ind_gg <- group_index[[gg]]
			}
			res_gg <- res2[ ind_gg, , drop=FALSE ]
			wgt_gg <- pweights0[ ind_gg ]
		}
		variance_gg <- tam_cov_wt(x=res_gg, wt=wgt_gg)
		variance[[gg]] <- variance_gg
	}			
	#--- OUTPUT
	res <- list(beta=beta, variance=variance)
	return(res)
}