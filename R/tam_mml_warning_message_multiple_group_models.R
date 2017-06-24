
tam_mml_warning_message_multiple_group_models <- function( ndim, G, disable=FALSE)
{
	if (disable){ G <- 1}
	if( (ndim>1) & ( G>1) ){
		v1 <- paste0("Multiple group estimation is not (yet) supported for \n",
			 "  multidimensional models. Use 'tam.mml.3pl' instead.")
		stop(v1)
	}
}