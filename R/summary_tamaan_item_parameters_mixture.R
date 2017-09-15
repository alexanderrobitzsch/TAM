## File Name: summary_tamaan_item_parameters_mixture.R
## File Version: 0.03
## File Last Change: 2017-09-15 09:59:49


#########################################################################
# item parameters
summary_tamaan_item_parameters_mixture <- function(object)
{
	cat("------------------------------------------------------------\n")
	cat("Item Parameters\n")
	obji <- object$itempartable_MIXTURE
	obji <- tam_round_data_frame_print(obji=obji, from=3, to=ncol(obji), digits=3)
	cat("------------------------------------------------------------\n")
	cat("\nGammaslope Parameters\n")
	obji <- object$gammaslope
	obji <- tam_round_data_frame_print(obji=obji, digits=3)
}
###################################################################################					
		
