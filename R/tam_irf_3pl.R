
tam_irf_3pl <- function(theta, AXsi, B, guess=NULL)
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
	probs <- tam_mml_3pl_calc_prob(iIndex=1:nitems, A=NULL, AXsi=AXsi, B=B, xsi=NULL, 
					theta=theta, nnodes=nnodes, maxK=maxK, recalc=FALSE , guess=guess)$rprobs
	probs <- aperm( probs , c(3,1,2) )
	#--- OUTPUT
	return(probs)
}
