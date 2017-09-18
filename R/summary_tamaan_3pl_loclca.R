## File Name: summary_tamaan_3pl_loclca.R
## File Version: 0.04
## File Last Change: 2017-09-18 10:35:35


##############################################res#################
# cluster locations
summary_tamaan_3pl_loclca <- function(object)
{
	#-- print cluster locations
	cat("*******************************\n")
	cat("Cluster locations\n")
	tam_round_data_frame_print(obji=object$locs, digits=3)	
	#-- print item response probabilities
	summary_tamaan_3pl_lcaprobs(object=object)
} 
######################################################################				
