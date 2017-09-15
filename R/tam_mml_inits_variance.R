## File Name: tam_mml_inits_variance.R
## File Version: 0.02
## File Last Change: 2017-05-25 14:51:52

tam_mml_inits_variance <- function( variance.inits, ndim , variance.fixed )
{
    # variance inits  
    # initialise conditional variance 
    if ( ! is.null( variance.inits ) ){
		variance <- variance.inits
    } else {
		variance <- diag( ndim ) 
	}
    if ( !is.null(variance.fixed) ){
		variance[ variance.fixed[,1:2 ,drop=FALSE] ] <- variance.fixed[,3]
		variance[ variance.fixed[,c(2,1) ,drop=FALSE] ] <- variance.fixed[,3]	
    }
	#--- OUTPUT
	res <- list( variance=variance )
	return(res)
}
	
