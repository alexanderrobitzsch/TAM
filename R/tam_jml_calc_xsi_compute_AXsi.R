## File Name: tam_jml_calc_xsi_compute_AXsi.R
## File Version: 0.03


tam_jml_calc_xsi_compute_AXsi <- function(A, xsi)
{
    dim_A <- dim(A)
    nitems <- dim_A[1]
    maxK <- dim_A[2]
    AXsi <- matrix(0, nrow=nitems, ncol=maxK)
    for (i in 1:nitems) {
        for (k in 1:maxK){
            AXsi[i,k] <- ( A[i,k,] %*% xsi )
        }
    }
    AXsi[is.na(AXsi)] <- -99
    return(AXsi)
}
