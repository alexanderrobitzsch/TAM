## File Name: weighted_curtosis.R
## File Version: 0.01
## File Last Change: 2017-04-29 18:12:15

#######################################################		
# curtosis
weighted_curtosis <- function( x , w = rep(1,length(x)) ){	
	m <- weighted_mean( x=x , w=w)
	v <- weighted_var( x=x, w=w)
	y <- (x-m)^4
	res <- weighted_mean(x=y, w=w) / v^2 - 3	
	return(res)
}
###########################################################		
