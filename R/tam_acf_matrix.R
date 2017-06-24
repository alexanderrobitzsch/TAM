

tam_acf_matrix <- function(x)
{
	# compute row-wise autocovariance
	# It corresponds to an autocorrelation if rows are standardized
	nx <- ncol(x)
	v1 <- rep(0, nrow(x) )
	for (nn in 2:nx){
		v1 <- x[,nn] * x[,nn-1]
	}
	v1 <- v1 / ( nx - 1)
	return(v1)
}