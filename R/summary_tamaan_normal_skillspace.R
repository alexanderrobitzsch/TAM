## File Name: summary_tamaan_normal_skillspace.R
## File Version: 0.01


###############################################################
# normal skill space
summary_tamaan_normal_skillspace <- function(object)
{
	cat("------------------------------------------------------------\n")
	cat("EAP Reliability\n")
	obji <- round( object$EAP.rel , 3 )
	print( obji )		
	cat("------------------------------------------------------------\n")
	cat("Covariances and Variances\n")
	if ( object$G >1){
		a1 <- stats::aggregate( object$variance , list( object$group ) , mean )
		object$variance <- a1[,2]
	}
	obji <- round( object$variance , 3 )
	if ( object$G >1){
		names(obji) <- paste0("Group" , object$groups )
	}		
	print( obji )
	cat("------------------------------------------------------------\n")
	cat("Correlations and Standard Deviations (in the diagonal)\n")
	if ( object$G >1){
		obji <- sqrt( object$variance )
	} else {
		obji <- stats::cov2cor(object$variance)
		diag(obji) <- sqrt( diag( object$variance) )
	}
	if ( object$G >1){
		names(obji) <- paste0("Group" , object$groups )			
	}		
	obji <- round( obji, 3 )
	print( obji )
	cat("------------------------------------------------------------\n")
	cat("Regression Coefficients\n")
	obji <- round( object$beta , 5 )
	print( obji )		
} 
######################################################################				

