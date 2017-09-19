## File Name: weighted_skewness.R
## File Version: 9.04
## File Last Change: 2017-09-19 15:35:03

#####################################################
# skewness
weighted_skewness <- function( x , w = rep(1,length(x)), select=NULL )
{	
    res <- tam_weighted_stats_select(x=x, w=w, select=select)	
	x <- res$x
	w <- res$w
	m <- weighted_mean( x=x , w=w)
	s <- weighted_sd( x=x, w=w)
	y <- ((x-m)/s)^3
	res <- weighted_mean(x=y, w=w)	
	return(res)
}
