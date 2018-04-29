## File Name: tam_mml_mstep_xsi.R
## File Version: 0.07

tam_mml_mstep_xsi <- function( max_increment , est.xsi.index0 , control ,
        np , nitems , A , AXsi , B , xsi , theta , nnodes , maxK , rprobs ,
        itemwt , indexIP.no , indexIP.list2 , Avector , ItemScore ,
        tam_function , xsi.fixed , iter , xsi_acceleration
        )
{
    #M-step
    oldxsi <- xsi
    Msteps <- control$Msteps
    fac.oldxsi <- control$fac.oldxsi
    convM <- control$convM
    converge <- FALSE
    Miter <- 1
    max_increment -> max.increment
    old_increment <- rep( max.increment , np )
    est.xsi.index <- est.xsi.index0

    #---------------------------
    while (!converge & ( Miter <= Msteps ) ) {
        z0 <- Sys.time()

        if (Miter > 1){
            res.p <- tam_mml_calc_prob( iIndex=1:nitems , A=A , AXsi=AXsi , B=B ,
                        xsi=xsi , theta=theta , nnodes=nnodes, maxK=maxK)
            rprobs <- res.p[["rprobs"]]
        }

        res <- tam_calc_exp( rprobs , A , np , est.xsi.index , itemwt ,
                    indexIP.no , indexIP.list2 , Avector )
        xbar <- res$xbar
        xbar2 <- res$xbar2
        xxf <- res$xxf

        # Compute the difference between sufficient statistic and expectation
        diff <- as.vector(ItemScore) - xbar
        #Compute the Newton-Raphson derivative for the equation to be solved
        deriv <- xbar2 - xxf
        increment <- diff*abs(1/( deriv + 1E-20 ) )
        if ( ! is.null( xsi.fixed) ){
            increment[ xsi.fixed[,1] ] <- 0
        }
        #!!!      necesessary to include statement to control increment?
        ci <- ceiling( abs(increment) / ( abs( old_increment) + 1E-10 ) )
        increment <- ifelse( abs( increment) > abs(old_increment)  ,
                            increment/(2*ci) ,
                            increment )
        #  increment <- ifelse( abs( increment) > abs(old_increment)  ,
        #                       sign(increment) * max.increment , increment )
        old_increment <- increment
        se.xsi <- sqrt( 1 / abs(deriv) )
        if ( ! is.null( xsi.fixed) ){
            se.xsi[ xsi.fixed[,1] ] <- 0
        }
        ##**
        xsi <- xsi+increment   # update parameter p
        # est.xsi.index <- which( abs(increment) > convM )

        if ( max(abs(increment)) < convM ) {
            converge <- TRUE
        }
        Miter <- Miter + 1

        # stabilizing the algorithm | ARb 2013-09-10
        if (fac.oldxsi > 0 ){
            xsi <-  (1-fac.oldxsi) * xsi + fac.oldxsi *oldxsi
        }
        # progress bar
        if ( control$progress){
            cat("-")
            utils::flush.console()
        }
    } # end Msteps
    #-------------------------
    max_increment <- max( abs( xsi - oldxsi )     )
    # acceleration
    if ( xsi_acceleration$acceleration != "none" ){
        xsi_acceleration <- accelerate_parameters( xsi_acceleration=xsi_acceleration ,
                        xsi=xsi , iter=iter , itermin=3)
        xsi <- xsi_acceleration$parm
    }
    #------ output
    res <- list( xsi = xsi , xsi_accleration = xsi_acceleration     ,
                se.xsi = se.xsi , max_increment = max_increment )
    return(res)
}

# cat("calc prob") ; z1 <- Sys.time(); print(z1-z0) ; z0 <- z1
