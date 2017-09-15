## File Name: tam_AXsi_compute.R
## File Version: 0.01
## File Last Change: 2017-06-15 13:56:16

tam_AXsi_compute <- function(A, xsi)
{
	dA <- dim(A)
	AX <- matrix( NA , nrow=dA[1] , ncol=dA[2] )
	for (kk in 1:( dA[2] ) ){
		AX[,kk] <- A[,kk,] %*% xsi
	}
	return(AX)
}
