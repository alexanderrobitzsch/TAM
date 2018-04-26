## File Name: tam_pv_mcmc_calc_probs_irf_3pl.R
## File Version: 0.28


tam_pv_mcmc_calc_probs_irf_3pl <- function(theta, AXsi, B, guess, 
		subtract_max=FALSE, resp.ind )	
{
	if ( is.vector(theta) ){
		theta <- matrix( theta , ncol=1 )
	}
	nnodes <- nrow(theta)
	nitems <- nrow(AXsi)
	maxK <- ncol(AXsi)
	if ( is.null(guess) ){
		guess <- rep(0,nitems)
	}
	
	#--- compute probabilities		
	I <- nitems
	res <- tam_rcpp_pv_mcmc_calc_probs_irf_3pl(theta=theta, B=as.vector(B), I=I, maxK=maxK,
				resp_ind = resp.ind, AXsi=AXsi )
	rprobs <- array( res$rprobs , dim=c(I,maxK, nnodes) )
		
	# include guessing	
	rprobs0 <- rprobs
	ind <- which(guess > 1E-6 )
	if ( length(ind) > 0 ){
		rprobs[ ind , 2 , ] <- guess[ind] + ( 1-guess[ind] ) * rprobs0[ind,2,]	
		# include guessing here
		rprobs[ ind , 1 , ] <- 1 - rprobs[ ind , 2 , ]
	}						
	probs <- rprobs
	probs <- aperm( probs , c(3,1,2) )		
	
	#--- OUTPUT
	return(probs)
}
