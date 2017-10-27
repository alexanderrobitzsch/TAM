## File Name: tam_pv_mcmc_sample_beta_variance_lm_beta.R
## File Version: 0.05

tam_pv_mcmc_sample_beta_variance_lm_beta <- function(Y0, theta0, pweights0, D, 
		use_lm=TRUE)
{
	if (use_lm){
		formula_theta <- theta0 ~ 0 + Y0	
		mod2 <- stats::lm( formula=formula_theta , weights= pweights0 )
		beta2 <- mod2$coef
		beta2 <- matrix( beta2 , ncol=D )
		res2 <- stats::resid(mod2)
	}
	if (! use_lm){
		mod <- stats::lm.wfit(y=theta0, x=Y0, w=pweights0)
		beta2 <- matrix( mod$coefficients , ncol=D)
		res2 <- matrix( mod$residuals , ncol=D )
	}		
	#--- output
	res <- list( beta2=beta2 , res2=res2)
	return(res)
}			
