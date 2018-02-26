## File Name: tam_mml_se_quick_likelihood.R
## File Version: 0.01


tam_mml_se_quick_likelihood <- function( nitems, A, AXsi, B, xsi, theta, nnodes, maxK,
		gwt, resp, resp.ind.list, snodes , thetawidth )
{
	# calculate probabilities
	res0 <- tam_mml_calc_prob( iIndex=1:nitems , A=A , AXsi=AXsi , B=B , 
							xsi=xsi , theta=theta , nnodes=nnodes , maxK=maxK )
	rprobs <- res0$rprobs
	# calculate likelihood
	like0 <- tam_calc_posterior(rprobs=rprobs , gwt=gwt , resp=resp , 
								nitems=nitems , resp.ind.list=resp.ind.list , 
								normalization = FALSE , thetasamp.density = NULL , 
								snodes = snodes )$hwt
	# calculate individual log likelihood
	res <- tam_mml_se_quick_compute_log_likelihood( like0=like0, 
									thetawidth=thetawidth, snodes=snodes )
	return(res)
}
