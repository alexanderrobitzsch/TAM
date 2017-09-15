## File Name: tam_rbind_twomatrices.R
## File Version: 9.03
## File Last Change: 2017-04-29 18:02:46

#########################################
# bind two matrices
tam_rbind_twomatrices <- function(X1 , X2){
	d1 <- dim(X1)
	d2 <- dim(X2)
	X3 <- matrix( 0 , nrow= d1[1] + d2[1] , ncol= d1[2] + d2[2] )
	X3[ 1:d1[1] , 1:d1[2] ] <- X1
	X3[ d1[1] + 1:d2[1] , d1[2] + 1:d2[2] ] <- X2
	return(X3)
}
#########################################

rbind.twomatrices <- tam_rbind_twomatrices 
