
tam_args_replace_value <- function( args , variable=NULL , value=NULL)
{
	if ( ! is.null(variable) ){
		args[[ variable ]] <- value
	}
	return(args)
}