## File Name: tam_AXsi_compute.R
## File Version: 0.02

tam_AXsi_compute <- function(A, xsi)
{
    dA <- dim(A)
    AX <- matrix( NA, nrow=dA[1], ncol=dA[2] )
    for (kk in 1:( dA[2] ) ){
        AX[,kk] <- A[,kk,] %*% xsi
    }
    return(AX)
}
