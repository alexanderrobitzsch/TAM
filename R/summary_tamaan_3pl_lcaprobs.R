## File Name: summary_tamaan_3pl_lcaprobs.R
## File Version: 0.01
## File Last Change: 2017-09-15 09:42:33


########################################################
summary_tamaan_3pl_lcaprobs <- function(object)
{
	cat("------------------------------------------------------------\n")
	cat("Item Response Probabilities\n")
	obji <- object$lcaprobs
	obji[,-1] <- round( obji[,-1] , 4 )	
	print(obji)
}
##########################################################

