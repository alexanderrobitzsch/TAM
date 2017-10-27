## File Name: tam_mml_mstep_intercept_optim.R
## File Version: 0.05

tam_mml_mstep_intercept_optim <- function( xsi, n.ik, prior_list_xsi, nitems, A,
		AXsi, B, theta, nnodes, maxK, Msteps, xsi.fixed, eps=1E-40)
{

	NX <- length(xsi)
	oldxsi <- xsi
	
	#----------------------------------------------------------------
	# define posterior function	
	posterior_xsi <- function(x){		
		#-- calculate expected log likelihood
		rprobs <- tam_mml_calc_prob( iIndex=1:nitems , A=A , AXsi=AXsi , B=B , 
							xsi=x , theta=theta , nnodes=nnodes, maxK=maxK,
							recalc=TRUE )$rprobs					
		G <- dim(n.ik)[4]
		counts <- array( 0 , dim=dim(n.ik)[1:3] )
		for (gg in 1:G){
			counts <- counts + n.ik[,,,gg]
		}
		rprobs <- aperm( rprobs, c(3,1,2) )
		rprobs[ is.na(rprobs) ] <- 0
		rprobs <- rprobs + eps	
		ll <- 0
		for (kk in 1:maxK){
			ll <- ll + sum( counts[,,kk] * log( rprobs[,,kk] ) )
		}
		#-- calculate prior distribution
		logprior <- tam_evaluate_prior( prior_list = prior_list_xsi , parameter = xsi, derivatives = FALSE )$d0
		#-- posterior distribution
		logpost <- ll + sum( logprior )		
		return( - logpost)
	}
	#----------------------------------------------------------------
	
	#-- optimitzation in optim
	lower <- rep(-Inf, NX)
	upper <- rep(Inf, NX)
	if (! is.null(xsi.fixed) ){
		eps0 <- 1e-4
		lower[ xsi.fixed[,1] ] <- xsi.fixed[,2] - eps0
		upper[ xsi.fixed[,1] ] <- xsi.fixed[,2] + eps0		
	}
	method <- "L-BFGS-B"
	
	args <- list( par = xsi, fn=posterior_xsi, method = method, 
					lower=lower, upper=upper, control=list(maxit=Msteps), 
					hessian=TRUE )
	res <- do.call( stats::optim , args)
	xsi <- res$par
	
	increment <- xsi - oldxsi	
	increment <- tam_trim_increment( increment=increment, max.increment=1, 
							trim_increment="cut")	
	xsi <- oldxsi + increment
	
	se.xsi <- sqrt( diag( solve( res$hessian ) ) )
	res <- tam_evaluate_prior( prior_list = prior_list_xsi , parameter = xsi )
	logprior_xsi <- res$d0	
	#--- output
	res <- list( xsi = xsi , se.xsi = se.xsi, logprior_xsi=logprior_xsi )
	return(res)
}
