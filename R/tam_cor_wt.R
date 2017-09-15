## File Name: tam_cor_wt.R
## File Version: 0.02
## File Last Change: 2017-08-22 17:57:14


tam_cor_wt <- function(x, wt=NULL, method="ML")
{
	if ( is.vector(x) ){
		x <- matrix(x, ncol=1)
	}
	D <- ncol(x)
	if ( is.null(wt) ){
		wt <- rep( 1 / nrow(x) , nrow(x) )	
	}
	x <- as.data.frame(x)
	variance_gg <- stats::cov.wt( x = x, wt = wt, cor=TRUE, method=method)$cor
	variance_gg <- matrix( variance_gg , nrow=D , ncol=D )
	return(variance_gg)
}
