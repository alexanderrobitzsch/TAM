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
