## File Name: tam_in_names_list.R
## File Version: 0.03
## File Last Change: 2017-08-13 22:25:40

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
