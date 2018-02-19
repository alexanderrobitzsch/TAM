## File Name: tam_latent_regression_standardized_solution.R
## File Version: 0.11

tam_latent_regression_standardized_solution <- function(variance, beta, Y)
{

	res <- NULL
	compute_stand <- TRUE
	if ( ncol(Y) == 1 ){
		compute_stand <- FALSE
	}
	if ( ! is.matrix(variance) ){
		compute_stand <- FALSE
	}
	
	if (compute_stand){
		#--- compute explained variance
		N <- nrow(Y)
		ND <- ncol(beta)
		Y_exp <- matrix(0, nrow=N, ncol=ND)
		var_y_exp <- rep(NA,ND)
		
		for (dd in 1:ND){
			Y_exp[,dd] <- Y %*% beta[,dd]
			var_y_exp[dd] <- stats::var( Y_exp[,dd] )
		}
		
		#--- sd theta
		sd_theta <- sqrt( var_y_exp + diag(variance) )
		R2_theta <- var_y_exp / sd_theta^2
		#--- standard deviations predictors
		sd_x <- apply(Y, 2 , stats::sd)
		#--- standardized coefficients
		NY <- ncol(Y)
		beta_stand <- matrix( NA, nrow=NY*ND, ncol=6 )
		colnames(beta_stand) <- c("parm", "dim", "est", "StdYX", "StdX", "StdY")
		beta_stand <- as.data.frame(beta_stand)
		beta_stand$parm <- rep( colnames(Y) , ND )
		sd_x0 <- sd_x
		sd_x0[ sd_x0 == 0 ] <- NA
		for (dd in 1:ND){
			ind_dd <- NY*(dd-1) + 1:NY
			beta_stand[ ind_dd , "dim"] <- dd
			beta_dd <- beta[,dd]
			beta_stand[ ind_dd , "est"] <- beta_dd
			beta_stand[ ind_dd , "StdX"] <- beta_dd * sd_x0
			beta_stand[ ind_dd , "StdY"] <- beta_dd / sd_theta *  ( sd_x0 > -10 )
			beta_stand[ ind_dd , "StdYX"] <- beta_dd / sd_theta * sd_x0
		}
		#--- output
		res <- list( beta_stand=beta_stand, R2_theta = R2_theta, sd_theta = sd_theta, sd_x = sd_x,
						var_y_exp=var_y_exp)			
	}				
	return(res)
}
