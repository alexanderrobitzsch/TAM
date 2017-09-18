## File Name: summary_tamaan_3pl_lcaprobs.R
## File Version: 0.03
## File Last Change: 2017-09-18 10:35:25


########################################################
summary_tamaan_3pl_lcaprobs <- function(object)
{
	cat("------------------------------------------------------------\n")
	cat("Item Response Probabilities\n")
	obji <- object$lcaprobs
	tam_round_data_frame_print(obji=obji, from=2, to=ncol(obji), digits=4)
}
##########################################################

