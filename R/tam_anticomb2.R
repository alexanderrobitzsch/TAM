## File Name: tam_anticomb2.R
## File Version: 0.01
## File Last Change: 2017-08-15 14:28:21

tam_anticomb2 <- function(H)
{
	D0 <- round( sqrt( 2*H + .25 ) - .5 )
	return(D0)
}
