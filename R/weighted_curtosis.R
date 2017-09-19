## File Name: weighted_curtosis.R
## File Version: 0.02
## File Last Change: 2017-09-19 15:36:58

#######################################################		
# curtosis
weighted_curtosis <- function( x , w = rep(1,length(x)), select=NULL )
{	
    res <- tam_weighted_stats_select(x=x, w=w, select=select)	
	x <- res$x
	w <- res$w
	m <- weighted_mean( x=x , w=w)
	v <- weighted_var( x=x, w=w)
	y <- (x-m)^4
	res <- weighted_mean(x=y, w=w) / v^2 - 3	
	return(res)
}
###########################################################		
