## File Name: tam_pv_mcmc_inits_plausible_values_objects.R
## File Version: 0.05
## File Last Change: 2017-05-31 13:55:34

tam_pv_mcmc_inits_plausible_values_objects <- function( n.burnin, n.iter,
		nplausible, D, nstud, pid )
{
	nplausible <- min( nplausible , n.iter - n.burnin)
	# iterations for saving plausible values
	pv_iter <- round( seq( n.burnin + 1 , n.iter , length=nplausible ) )
	# create object for saving PVs
	pv <- matrix( NA , nrow=nstud , ncol=D*nplausible + 1 )
	cn_pv <- "pid"
	pv_index_matrix <- list()	
	for (pp in 1:nplausible){
		cn_pv <- c( cn_pv , paste0("PV" , pp , ".Dim" , 1:D) )
		pv_index_matrix[[pp]] <- 1 + D*(pp-1) + 1:D
	}
	pv <- as.data.frame(pv)
	colnames(pv) <- cn_pv
	pv$pid <- pid
	attr(pv, "last_pv") <- 0
	attr(pv, "nplausible") <- nplausible
	attr(pv, "D") <- D
	attr(pv, "pv_iter") <- pv_iter
	attr(pv, "pv_lag") <- round( mean( diff(pv_iter) ) )
	#--- OUTPUT
	res <- list( pv = pv, pv_iter=pv_iter, pv_index_matrix=pv_index_matrix,
					nplausible=nplausible)
	return(res)
}
