## File Name: tam_pv_mcmc_parameter_summary.R
## File Version: 0.08

tam_pv_mcmc_parameter_summary <- function(parameter_samples, level)
{
	NP <- ncol(parameter_samples)
	parnames <- colnames(parameter_samples)
	dfr <- data.frame("parm" = parnames , 
				"est" = colMeans(parameter_samples) )
	dfr$se <- apply( parameter_samples, 2 , stats::sd )
	#--- credibility interval
	dfr$low <- apply( parameter_samples, 2 , stats::quantile , prob = (1-level)/2 , na.rm=TRUE)
	dfr$upp <- apply( parameter_samples, 2 , stats::quantile , prob = 1 - (1-level)/2, na.rm=TRUE)
	some_miss <- sum( is.na(parameter_samples) ) > 0
	#--- effective sample size
	if ( some_miss){ 
		dfr$ESS <- NA
	} else {
		dfr$ESS <- round( coda::effectiveSize( x=parameter_samples ), 1)
	}
	#--- Rhat statistic
	dfr$Rhat <- tam_Rhat_3splits(parameter_samples=parameter_samples)	
	#--- OUTPUT
	rownames(dfr) <- NULL		
	return(dfr)			
}
