## File Name: tam_calc_prob_helper_subtract_max.R
## File Version: 0.05

tam_calc_prob_helper_subtract_max <- function( rr0 )
{
	RR0 <- dim(rr0)
	rr0M <- matrix( rr0 , nrow=RR0[1]*RR0[2] , ncol=RR0[3] )
	NI <- RR0[1]
	NK <- RR0[2]
	TP <- RR0[3]	
	rr1M <- tam_rcpp_calc_prob_subtract_max( rr0M=rr0M, NI=NI, NK=NK, TP=TP ) 
	rr1 <- array( rr1M , dim = RR0 )
	return(rr1)
}
