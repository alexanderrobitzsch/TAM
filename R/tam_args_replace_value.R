## File Name: tam_args_replace_value.R
## File Version: 0.01
## File Last Change: 2017-04-19 15:52:07

tam_args_replace_value <- function( args , variable=NULL , value=NULL)
{
	if ( ! is.null(variable) ){
		args[[ variable ]] <- value
	}
	return(args)
}
