## File Name: tam_pv_mcmc_proc_regressors.R
## File Version: 0.03

tam_pv_mcmc_proc_regressors <- function(Y)
{
	if (is.null(colnames(Y))){
		colnames(Y) <- paste0( "Y" , 1:ncol(Y))
	}
	return(Y)
}
