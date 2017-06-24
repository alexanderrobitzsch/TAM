
tam_vec2symmmatrix <- function(variance)
{
	H <- length(variance)
	D0 <- round( sqrt( 2*H + .25 ) - .5 )
	variance0 <- matrix( NA , nrow=D0, ncol=D0)	
	variance0[ ! lower.tri(variance0) ] <- variance
	variance0[ ! upper.tri(variance0) ] <- variance
	return(variance0)
}