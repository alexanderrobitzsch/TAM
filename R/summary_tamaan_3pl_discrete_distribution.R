## File Name: summary_tamaan_3pl_discrete_distribution.R
## File Version: 0.01
## File Last Change: 2017-09-15 09:43:20



############################################################
# discrete distributions
summary_tamaan_3pl_discrete_distribution <- function(object)
{
	# latent class distributions
	cat("------------------------------------------------------------\n")
	cat("Trait distribution parameters delta\n")
	obji <- round( object$delta , 4 )
	colnames(obji) <- paste0("Group" , 1:object$G)
	print( obji )		
	TP <- nrow(obji)			
	cat("------------------------------------------------------------\n")
	cat("Full Trait distribution\n")
	obji <- round( object$pi.k , 4 )			
	colnames(obji) <- paste0("Group" , 1:object$G)
	print( obji )		
}
############################################################	
