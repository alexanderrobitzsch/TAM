## File Name: weighted_mean.R
## File Version: 9.11
############################################
# weighted mean
weighted_mean <- function( x , w=rep(1,length(x)), select=NULL ){
    res <- tam_weighted_stats_select(x=x, w=w, select=select)	
	x <- res$x
	w <- res$w
	res <- sum( x * w ) / sum(w)
	return(res)
}
###############################################
