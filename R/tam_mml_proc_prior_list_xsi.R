## File Name: tam_mml_proc_prior_list_xsi.R
## File Version: 0.02
## File Last Change: 2017-06-16 18:10:05

tam_mml_proc_prior_list_xsi <- function( prior_list_xsi , xsi )
{
	NX <- length(xsi)
	#-- some prior distributions
	is_prior <- ! is.null(prior_list_xsi)
	if (!is_prior){
		prior_list_xsi <- list()
	}	
	#-- entries with some prior distribution
	prior_entries <- NULL
	if (is_prior){		
		for (pp in 1:( length(prior_list_xsi) )){
			if ( ! is.null( prior_list_xsi[[pp]] ) ){
				prior_entries <- c( prior_entries, pp )
				prior_list_xsi[[pp]][[2]][["x"]] <- NA
			}
		}	
	}
	#--- set some attributes
	attr( prior_list_xsi , "dim_parameter" ) <- NX
	attr( prior_list_xsi , "is_prior" ) <- is_prior
	attr( prior_list_xsi , "prior_entries" ) <- prior_entries
	attr( prior_list_xsi , "length_prior_entries" ) <- length(prior_entries)
	#--- output
	return(prior_list_xsi)
}
