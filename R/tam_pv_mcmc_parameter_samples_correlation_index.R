## File Name: tam_pv_mcmc_parameter_samples_correlation_index.R
## File Version: 0.07

tam_pv_mcmc_parameter_samples_correlation_index <- function(index)
{
    H <- length(index)
    D <- tam_anticomb2(H=H)
    mat <- matrix(NA, nrow=H, ncol=3)
    colnames(mat) <- c("index","dim1","dim2")
    mat <- as.data.frame(mat)
    mat$index <- 1:H
    hh <- 1
    for (dd in 1:D){
        for (ee in dd:D){
            mat[hh,"dim1"] <- dd
            mat[hh,"dim2"] <- ee
            hh <- hh + 1
        }
    }
    mat$index2 <- 1 *( mat$dim1 !=mat$dim2 )
    mat$index2 <- cumsum(mat$index2)
    mat$index2[ mat$dim1==mat$dim2 ] <- 0
    return(mat)
}
