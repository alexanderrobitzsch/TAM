## File Name: tam_mml_3pl_expected_counts.R
## File Version: 0.01


##########################################################################
# calculation of expected counts
tam_mml_3pl_expected_counts <- function( datindw , nitems , maxK , ntheta , hwt){
	# calculate expected counts
	n.ik <- array( 0 , dim=c(nitems , maxK , ntheta ) )
	N.ik <- array( 0 , dim=c( nitems , ntheta ) )	
	for (kk in 1:maxK){   # kk <- 1
		dkk <- datindw[[kk]]
		g1 <- crossprod( dkk , hwt ) 
		n.ik[,kk,] <- g1
		N.ik <- N.ik + g1
	}
	res <- list("n.ik"=n.ik , "N.ik" = N.ik )
	return(res)
}
#####################################################################

