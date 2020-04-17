## File Name: tam_mml_calc_prob_R.R
## File Version: 0.1684


tam_mml_calc_prob_R <- function(iIndex, A, AXsi, B, xsi, theta,
            nnodes, maxK, recalc=TRUE, avoid_outer=FALSE )
{
    D <- ncol(theta)
    if(recalc){
        LI <- length(iIndex)
        LXsi <- dim(A)[3]
        AXsi.tmp <- array( 0, dim=c( LI, maxK, nnodes ) )
        for (kk in 1:maxK){
            A_kk <- matrix( A[ iIndex, kk, ], nrow=LI, ncol=LXsi )
            AXsi.tmp[, kk, 1:nnodes ] <- A_kk %*% xsi
        }
        AXsi[iIndex,] <- AXsi.tmp[,,1]
    } else {
        # AXsi.tmp <- array( AXsi, dim=c( length(iIndex), maxK, nnodes ) )
        AXsi.tmp <- array( AXsi[ iIndex, ], dim=c( length(iIndex), maxK, nnodes ) )
    }
    dim_Btheta <- c(length(iIndex), maxK, nnodes)
    Btheta <- array(0, dim=dim_Btheta )
    for( dd in 1:D ){
        B_dd <- B[iIndex,,dd,drop=FALSE]
        theta_dd <- theta[,dd]
        if (! avoid_outer){
            Btheta_add <- array(B_dd %o% theta_dd, dim=dim(Btheta))
        } else {
            Btheta_add <- tam_rcpp_tam_mml_calc_prob_R_outer_Btheta( Btheta=Btheta,
                        B_dd=B_dd, theta_dd=theta_dd, dim_Btheta=dim_Btheta )
            Btheta_add <- array(Btheta_add, dim=dim_Btheta)
        }
        Btheta <- Btheta + Btheta_add
    }
    #*** subtract maximum in Rcpp to avoid numerical overflow
    rr0 <- Btheta + AXsi.tmp
    dim_rr <- dim(rr0)

    # rr1 <- tam_calc_prob_helper_subtract_max( rr0=rr0 )
    # rr <- exp(rr1)
    rr <- tam_rcpp_calc_prob_subtract_max_exp( rr0=rr0, dim_rr=dim_rr )

    
    #    rprobs <- rr / aperm( array( rep( colSums( aperm( rr, c(2,1,3) ),
    #                dims=1, na.rm=TRUE), maxK ), dim=dim(rr)[c(1,3,2)] ), c(1,3,2) )
    rprobs <- tam_rcpp_tam_mml_calc_prob_R_normalize_rprobs( rr=rr, dim_rr=dim_rr)
    rprobs <- array(rprobs, dim=dim_rr)

    #---- output
    res <- list("rprobs"=rprobs, "AXsi"=AXsi)
    return(res)
}
