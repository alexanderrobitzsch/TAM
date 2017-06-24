
tam_pv_mcmc_sample_beta_variance <- function( theta , Y, nstud , pweights, samp.regr=FALSE,
		G, group_index, beta_groups)
{

	# formula_theta <- theta ~ 0 + Y	
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
		mod2 <- stats::lm( formula=formula_theta , weights= pweights0 )
		beta2 <- mod2$coef
		beta2 <- matrix( beta2 , ncol=D )
		res2 <- stats::resid(mod2)	
		beta <- beta2		
	}
	variance <- list()
	for (gg in 1:G){	
		ind_gg <- group_index[[gg]]
		if (beta_groups){	
			Y0 <- as.matrix( Y[ ind_gg , , drop=FALSE] )
			theta0 <- theta[ ind_gg, , drop=FALSE ]
			pweights0 <- pweights[ind_gg]
			mod2 <- stats::lm( formula=formula_theta , weights= pweights0 )
			beta2 <- mod2$coef
			beta2 <- matrix( beta2 , ncol=D )
			res2 <- stats::resid(mod2)
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
		variance_gg <- stats::cov.wt( x = as.data.frame(res_gg), wt = wgt_gg , method="ML")$cov
		variance_gg <- matrix( variance_gg , nrow=D , ncol=D )
		variance[[gg]] <- variance_gg
	}			
	#--- OUTPUT
	res <- list(beta=beta, variance=variance)
	return(res)
}