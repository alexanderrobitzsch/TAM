
###########################################################################
# reduced skillspace estimation
tam_mml_3pl_skillspace <- function( Ngroup, pi.k , 
			delta.designmatrix , G , delta , delta.fixed ,			
			eps=1E-10, hwt, resp.ind , pweights, pweightsM,
			group1.list, delta_acceleration, iter)
{		
	Z <- delta.designmatrix	
	delta0 <- delta
	ND <- nrow(delta)
	covdelta <- list(1:G)
	
	itemwt <- crossprod( hwt , resp.ind * pweightsM  )	
	
	#-- group specific skill proportions
	for (gg in 1:G){
		ind.gg <- group1.list[[gg]]
		pi.k[,gg] <- colSums( ( pweights*hwt )[ind.gg,] )
		pi.k[,gg] <- pi.k[,gg] / sum( pi.k[,gg] )
	}		
	#-- skill space smoothing
	for (gg in 1:G){
		ntheta1 <- Ngroup[gg] * pi.k[,gg]
		ntheta1 <- ntheta1 / sum(ntheta1 )	
		lntheta <- log(ntheta1+eps)
		mod <- stats::lm( lntheta ~ 0 + Z , weights = ntheta1 )
		covbeta <- stats::vcov(mod)		
		beta <- stats::coef(mod)
		if ( ! is.null( delta.fixed ) ){
			# delta.fixed: 1st column: parameter index
			#              2nd column: group index
			#              3rd column: parameter value 
		    ind.gg <- which( delta.fixed[ ,2] == gg )
			if ( length(ind.gg) > 0 ){
				beta[ delta.fixed[ind.gg,1] ] <- delta.fixed[ind.gg,3]
			}
		}
		pi.k[,gg] <- exp( Z %*% beta ) / Ngroup[gg]
		pi.k[,gg] <- pi.k[,gg] / sum( pi.k[,gg] )
		delta[,gg] <- beta
		covdelta[[gg]] <- covbeta
	}			
	#----- delta acceleration
	if ( delta_acceleration$acceleration != "none" ){		
		delta_acceleration <- tam_accelerate_parameters( xsi_acceleration=delta_acceleration , 
						xsi=delta , iter=iter , itermin=3)
		delta <- matrix( delta_acceleration$parm , nrow=nrow(delta) , ncol=ncol(delta) )
	}			
	#---- delta parameter change
	delta_change <- tam_parameter_change( delta , delta0 )
	
	#---- OUTPUT
	res <- list( "pi.k"=pi.k , "delta"=delta , 
			"covdelta" = covdelta, itemwt = itemwt , delta_acceleration=delta_acceleration,
			delta_change = delta_change ,
			varConv = TRUE, betaConv=TRUE)			
	return(res)
}


.mml.3pl.skillspace <- tam_mml_3pl_skillspace
