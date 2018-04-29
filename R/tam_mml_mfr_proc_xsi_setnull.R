## File Name: tam_mml_mfr_proc_xsi_setnull.R
## File Version: 0.02

tam_mml_mfr_proc_xsi_setnull <- function(xsi.setnull, A, xsi.fixed)
{
    xsi0 <- NULL
    # set some xsi effects to null
    if ( ! is.null(xsi.setnull) ){
        xsi.labels <- dimnames(A)[[3]]
        N1 <- length(xsi.setnull)
        for (nn in 1:N1){
            ind.nn <- grep( xsi.setnull[nn] , xsi.labels )
            l1 <- cbind( ind.nn , 0 )
            xsi0 <- rbind( xsi0 , l1 )
            colnames(xsi0) <- NULL
        }
        xsi.fixed <- rbind( xsi.fixed , xsi0 )
        i2 <- duplicated(xsi.fixed[,1])
        if ( sum(i2) > 0 ){
            xsi.fixed <- xsi.fixed[ - i2  , ]
        }
    }
    #--- OUTPUT
    res <- list(xsi.fixed=xsi.fixed, xsi0=xsi0)
    return(res)
}
