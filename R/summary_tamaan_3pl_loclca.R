## File Name: summary_tamaan_3pl_loclca.R
## File Version: 0.01
## File Last Change: 2017-09-15 10:01:08


##############################################res#################
# cluster locations
summary_tamaan_3pl_loclca <- function(object)
{
	cat("*******************************\n")
	cat("Cluster locations\n")
	obji <- round( object$locs, 3 )
	print( obji )
	# item response probabilities
	summary_tamaan_3pl_lcaprobs(object=object)
} 
######################################################################				
