## File Name: tam_fa_reliability_measures.R
## File Version: 0.07

tam_fa_reliability_measures <- function( B.stand, itemvariance, xsi, maxK)
{
    g0 <- B.stand[,1] %*% t(B.stand[,1])
    g1 <- B.stand[,-1] %*% t(B.stand[,-1])
    g2 <- diag(1-rowSums( B.stand^2 ))
    ECV <- sum( B.stand[,1]^2 ) / sum( B.stand^2 )
    meas <- c(  "ECV"=ECV,
                "omega_a"=sum(g0) / sum(g0+g1),
                "omega_t"=sum(g0+g1) / sum(g0+g1+g2),
                "omega_h"=sum(g0) / sum(g0+g1+g2) )
    # omega_tot
    if (maxK==1){
        meas["omega_tot_diff"] <- tam_fa_reliability_nonlinearSEM(facloadings=B.stand,
                    thresh=- xsi[,1] / sqrt( itemvariance ) )$omega.rel
    }
    return(meas)
}
