## File Name: tam_args_CALL_search.R
## File Version: 0.04
## File Last Change: 2017-07-24 15:02:20

tam_args_CALL_search <- function(args_CALL , variable , default_value)
{
	res <- default_value
	if ( variable %in% names(args_CALL) ){
		res <- args_CALL[[ variable ]]
	}
	return(res)
}
