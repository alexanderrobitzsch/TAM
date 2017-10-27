## File Name: tam_in_names_list.R
## File Version: 0.03

tam_in_names_list <- function( list, variable )
{
	res <- FALSE
	names1 <- names(list)
	if ( ! is.null(names1) ){
		if ( variable %in% names1){
			res <- TRUE
		}	
	}
	return(res)
}
