## File Name: tam_pv_mcmc_parameter_summary.R
## File Version: 0.02

tam_pv_mcmc_parameter_summary <- function(parameter_samples, level)
{
	NP <- ncol(parameter_samples)
	parnames <- colnames(parameter_samples)
	dfr <- data.frame("parm" = parnames , 
				"est" = colMeans(parameter_samples) )
	dfr$se <- apply( parameter_samples, 2 , stats::sd )
	#--- credibility interval
	dfr$low <- apply( parameter_samples, 2 , stats::quantile , prob = (1-level)/2 )
	dfr$upp <- apply( parameter_samples, 2 , stats::quantile , prob = 1 - (1-level)/2 )
	#--- effective sample size
	dfr$ESS <- round( coda::effectiveSize( parameter_samples ), 1)
	#--- Rhat statistic
	dfr$Rhat <- tam_Rhat_3splits(parameter_samples=parameter_samples)	
	#--- OUTPUT
	rownames(dfr) <- NULL		
	return(dfr)			
}
