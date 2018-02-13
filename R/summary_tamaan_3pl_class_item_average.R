## File Name: summary_tamaan_3pl_class_item_average.R
## File Version: 0.02

########################################################
summary_tamaan_3pl_class_item_average <- function(object)
{
	cat("------------------------------------------------------------\n")
	cat("Class-Specific Item Means\n")
	obji <- object$lca_M
	tam_round_data_frame_print(obji=obji, from=2, to=ncol(obji), digits=4)
}
##########################################################
