## File Name: tam_mml_warning_message_multiple_group_models.R
## File Version: 0.02
## File Last Change: 2017-04-29 14:57:57

tam_mml_warning_message_multiple_group_models <- function( ndim, G, disable=FALSE)
{
	if (disable){ G <- 1}
	if( (ndim>1) & ( G>1) ){
		v1 <- paste0("Multiple group estimation is not (yet) supported for \n",
			 "  multidimensional models. Use 'tam.mml.3pl' instead.")
		stop(v1)
	}
}
