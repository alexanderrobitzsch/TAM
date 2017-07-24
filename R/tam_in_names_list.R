
tam_in_names_list <- function( list, variable )
{
	res <- FALSE
	names1 <- names(list)
	if ( ! is.null(names1) ){
		if ( names1 %in% variable){
			res <- TRUE
		}	
	}
	return(res)
}