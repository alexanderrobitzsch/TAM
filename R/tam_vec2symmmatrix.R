## File Name: tam_vec2symmmatrix.R
## File Version: 0.08

tam_vec2symmmatrix <- function(variance)
{
	H <- length(variance)
	D0 <- tam_anticomb2(H=H)
	variance0 <- matrix( NA , nrow=D0, ncol=D0)	
	ii <- 1
	for (dd in 1:D0){
		for (ee in dd:D0){
			variance0[dd,ee] <- variance0[ee,dd] <- variance[ii]
			ii <- ii+1
		}
	}
	return(variance0)
}
