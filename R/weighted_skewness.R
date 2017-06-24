
#####################################################
# skewness
weighted_skewness <- function( x , w = rep(1,length(x)) ){	
	m <- weighted_mean( x=x , w=w)
	s <- weighted_sd( x=x, w=w)
	y <- ((x-m)/s)^3
	res <- weighted_mean(x=y, w=w)	
	return(res)
}
