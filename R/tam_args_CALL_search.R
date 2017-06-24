
tam_args_CALL_search <- function(args_CALL , variable , default_value)
{
	res <- default_value
	if ( variable %in% names(args_CALL) ){
		res <- args_CALL[[ variable ]]
	}
	return(res)
}