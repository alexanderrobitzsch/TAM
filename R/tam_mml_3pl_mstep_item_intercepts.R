## File Name: tam_mml_3pl_mstep_item_intercepts.R
## File Version: 9.50


##########################################################################
# estimation of item intercepts
tam_mml_3pl_mstep_item_intercepts <-
    function( max.increment, np, est.xsi.index0,
        Msteps, nitems, A, AXsi, B, xsi, guess, theta, nnodes, maxK,
        progress, itemwt, indexIP.no, indexIP.list2,
        ItemScore, fac.oldxsi, rprobs, xsi.fixed, convM, rprobs0,
        n.ik, N.ik, xsi.prior, indexIP.list, xsi_acceleration, iter)
{
    converge <- FALSE
    Miter <- 1
    if (progress){
        cat("M Step Intercepts   |")
        flush.console()
    }

    eps <- 1e-10
    oldxsi <- xsi
    old_increment <- rep( max.increment, np )
    est.xsi.index <- est.xsi.index0

    #***************************************************
    while ( !converge & ( Miter <=Msteps ) ) {
        #      xbar2 <- xxf <- xbar <- rep(0,np)
        # numerical differentiation parameter
        h <- 1E-4

        #----- switch with respect to existence of guessing parameter
        guess_exists <- max( guess ) > eps

        if ( ! guess_exists ){
            # Only compute probabilities for items contributing to param p
            if (Miter > 1){
              res.p <- tam_mml_3pl_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B,
                                     xsi=xsi, theta=theta, nnodes=nnodes, maxK=maxK,
                                     guess=guess )
              rprobs <- res.p[["rprobs"]]
              rprobs0 <- res.p$rprobs0
            }
            res <- tam_mml_3pl_calc_exp( rprobs=rprobs, A=A, np=np, est.xsi.index=est.xsi.index,
                        itemwt=itemwt, indexIP.no=indexIP.no, indexIP.list2=indexIP.list2,
                        rprobs0=rprobs0, guess=guess, n.ik=n.ik, N.ik=N.ik )
            xbar <- res$xbar
            xbar2 <- res$xbar2
            xxf <- res$xxf
            ItemScore <- res$iscore
            # Compute the difference between sufficient statistic and expectation
            diff <- as.vector(ItemScore) - xbar
            #Compute the Newton-Raphson derivative for the equation to be solved
            deriv <- xbar2 - xxf
        }
        if (guess_exists){
            NX <- length(xsi)
            ll0 <- rep( NA, NX )
            ll1m <- ll1p <- NA*ll0
            iIndex <- 1:nitems
            for (xx in 1:NX){
                # xx <- 1
                # iIndex <- indexIP.list[[xx]]
                ll0[xx] <- tam_mml_3pl_calc_total_ll( iIndex=iIndex, A=A, B=B,
                                xsi=xsi, theta=theta,    nnodes=nnodes, guess=guess,
                                n.ik=n.ik, eps=eps )
                xsi1 <- tam_mml_3pl_vec_add_increment( vec=xsi, h=h, index=xx )
                ll1p[xx] <- tam_mml_3pl_calc_total_ll( iIndex=iIndex, A=A, B=B,
                                xsi=xsi1, theta=theta, nnodes=nnodes, guess=guess,
                                n.ik=n.ik, eps=eps )
                xsi2 <- tam_mml_3pl_vec_add_increment( vec=xsi, h=-h, index=xx )
                ll1m[xx] <- tam_mml_3pl_calc_total_ll( iIndex=iIndex, A=A, B=B,
                                xsi=xsi2, theta=theta, nnodes=nnodes, guess=guess,
                                n.ik=n.ik, eps=eps )
            }
            res <- tam_difference_quotient( d0=ll0, d0p=ll1p, d0m=ll1m, h=h)
            diff <- res$d1
            deriv <- res$d2
        }

        #***********************
        # xsi prior
          if ( ! is.null(xsi.prior) ){
                d0  <- log( stats::dnorm( xsi, mean=xsi.prior[,1],
                                sd=xsi.prior[,2] ) + eps)
                d0p  <- log( stats::dnorm( xsi + h, mean=xsi.prior[,1],
                                sd=xsi.prior[,2] ) + eps)
                d0m  <- log( stats::dnorm( xsi - h, mean=xsi.prior[,1],
                                sd=xsi.prior[,2] ) + eps)
                # d1 <- ( d0p - d0 ) / h
                # d2 <- ( ( d0p - d0 ) - ( d0 - d0m ) ) / h^2
                res <- tam_difference_quotient( d0=d0, d0p=d0p, d0m=d0m, h=h)
                d1 <- res$d1
                d2 <- res$d2
                diff <- diff + d1
                deriv <- deriv + d2
            }
        #************************

        increment <- diff*abs(1/( deriv + 10^(-20) ) )
        if ( !is.null( xsi.fixed) ){
            increment[ xsi.fixed[,1] ] <- 0
        }
        increment <- tam_trim_increment(increment=increment,
                            max.increment=abs(old_increment), trim_increment="half")
        old_increment <- increment

        ##**SE
        se.xsi <- sqrt( 1 / abs(deriv) )
        if ( ! is.null( xsi.fixed) ){ se.xsi[ xsi.fixed[,1] ] <- 0 }
        ##**

        xsi <- xsi+increment   # update parameter p
        #      est.xsi.index <- which( abs(increment) > convM )
        if ( max(abs(increment)) < convM ) { converge <- TRUE }
        Miter <- Miter + 1

        # stabilizing the algorithm | ARb 2013-09-10
        if (fac.oldxsi > 0 ){
            xsi <-  (1-fac.oldxsi) * xsi + fac.oldxsi *oldxsi
        }

        # progress bar
        if (progress){
            cat("-")
            utils::flush.console()
        }
    } # end of all parameters loop
    #------
    # -- xsi acceleration
    if ( xsi_acceleration$acceleration !="none" ){
        xsi_acceleration <- tam_accelerate_parameters( xsi_acceleration=xsi_acceleration,
                                xsi=xsi, iter=iter, itermin=3)
        xsi <- xsi_acceleration$parm
    }
    #--- parameter change
    xsi_change <- tam_parameter_change(xsi, oldxsi)
    #--- OUTPUT
    res <- list( "xsi"=xsi, "se.xsi"=se.xsi, xsi_change=xsi_change,
                    xsi_acceleration=xsi_acceleration)
    return(res)
}
#######################################################################

tam.mml.3pl.est.intercepts <- tam_mml_3pl_mstep_item_intercepts
.mml.3pl.est.intercepts <- tam_mml_3pl_mstep_item_intercepts

