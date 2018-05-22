## File Name: prior_list_include.R
## File Version: 0.03

prior_list_include <- function( prior_list, prior, index )
{
    if ( missing(prior_list) ){
        prior_list <- list()
    }
    for (ii in index){
        prior_list[[ ii ]] <- prior
    }
    return(prior_list)
}
