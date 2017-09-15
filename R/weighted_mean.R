## File Name: weighted_mean.R
## File Version: 9.07
## File Last Change: 2017-06-02 10:38:02
############################################
# weighted mean
weighted_mean <- function( x , w=rep(1,length(x)) ){
	ind <- ! is.na(x)
	x <- x[ind]
	w <- w[ind]
	res <- sum( x * w ) / sum(w)
	return(res)
}
###############################################
