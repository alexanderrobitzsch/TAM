
tam_bayesian_bootstrap <- function(N, do_boot=TRUE)
{
	if (do_boot){
		y <- stats::runif(N-1, min=0, max=1)
		y <- N * c(0 , sort(y) , 1 )
		v <- diff(y)
	} else {
		v <- rep(1,N)
	}
	return(v)
}