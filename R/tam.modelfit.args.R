## File Name: tam.modelfit.args.R
## File Version: 0.01
## File Last Change: 2017-09-15 17:13:08


########################################################
# tam.modelfit with user defined input
tam.modelfit.args <- function( resp , probs , theta , post , progress=TRUE )
{
	resp.ind <- as.matrix( 1- is.na(resp) )
	tamobj <- list( resp = resp , rprobs = probs , 
					theta = theta , hwt = post ,
					resp.ind = resp.ind )				
	res <- tam.modelfit( tamobj=tamobj , progress=progress)			
	return(res)																						
}
########################################################		
