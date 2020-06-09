## File Name: tam_jml_compute_Axsi.R
## File Version: 0.03

tam_jml_compute_Axsi <- function(A, xsi, resp)
{
    dim_A <- dim(A)
    K <- dim_A[2]
    I <- dim_A[1]
    AXsi <- matrix(NA, nrow=I, ncol=K)
    colnames(AXsi) <- paste0("Cat",0:(K-1) )
    rownames(AXsi) <- colnames(resp)
    for (kk in 1:K){
        AXsi[,kk] <- A[,kk,] %*% xsi
    }
    return(AXsi)
}
