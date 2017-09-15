## File Name: prior_list_include.R
## File Version: 0.01
## File Last Change: 2017-06-16 17:36:33

prior_list_include <- function( prior_list , prior , index )
{
	if ( missing(prior_list) ){
		prior_list <- list()
	}
	for (ii in index){
		prior_list[[ ii ]] <- prior
	}	
	return(prior_list)
}
