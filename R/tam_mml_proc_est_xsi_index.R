## File Name: tam_mml_proc_est_xsi_index.R
## File Version: 0.04

tam_mml_proc_est_xsi_index <- function(A, xsi.inits, xsi.fixed)
{
    # number of parameters
    np <- dim(A)[[3]]
    # xsi inits
    if ( ! is.null(xsi.inits) ){
        xsi <- rep(0,np)
        xsi[ xsi.inits[,1] ] <- xsi.inits[,2]
    } else {
        xsi <- rep(0,np)
    }
    if ( ! is.null( xsi.fixed ) ){
        xsi[ xsi.fixed[,1] ] <- xsi.fixed[,2]
        est.xsi.index <- setdiff( 1:np, xsi.fixed[,1] )
    } else {
        est.xsi.index <- 1:np
    }
    est.xsi.index0 <- est.xsi.index
    #--- OUTPUT
    res <- list( np=np, xsi=xsi, est.xsi.index=est.xsi.index )
    return(res)
}
