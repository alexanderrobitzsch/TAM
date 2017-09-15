## File Name: tam_mml_update_stochastic_nodes.R
## File Version: 0.01
## File Last Change: 2017-04-29 17:26:00

tam_mml_update_stochastic_nodes <- function(theta0.samp, variance, snodes, beta,
		theta)
{
	#-- compute new mean for each person
	theta <- beta[ rep(1,snodes) , ] + theta0.samp %*% chol(variance) 		
	# calculate density for all nodes
	thetasamp.density <- mvtnorm::dmvnorm( theta , mean = as.vector(beta[1,]) ,	sigma = variance )	
	# recalculate theta^2
	theta2 <- tam_theta_sq(theta=theta, is_matrix=TRUE)
	#--- OUTPUT
	res <- list( theta=theta, theta2=theta2, thetasamp.density=thetasamp.density)
	return(res)
}
