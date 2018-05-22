## File Name: tam_mml_3pl_mstep_item_slopes.R
## File Version: 9.63

########################################
# tam.mml.3pl estimate item slopes
tam_mml_3pl_mstep_item_slopes <- function( max.increment, np,
            Msteps, nitems, A, AXsi, B, xsi, guess, theta, nnodes, maxK,
            progress,ItemScore, fac.oldxsi, rprobs, xsi.fixed, convM, rprobs0,
            n.ik, N.ik, gammaslope, E, FdesM, dimFdes,
            gammaslope.fixed, gammaslope.prior, maxgamma=9.99, Edes,
            gammaslope.constr.V, V1, e2, gammaslope.center.index,
            gammaslope.center.value, userfct.gammaslope, gammaslope_acceleration, V )
{
    if (progress){
        cat("\nM Step Slopes       |")
        utils::flush.console()
    }
    eps <- 1e-10
    Nlam <- length(gammaslope)
    gammaslope0 <- Xlambda00 <- Xlambda0 <- Xlambda <- gammaslope
    Xlambda.fixed <- gammaslope.fixed
    msteps <- Msteps
    parchange <- 1
    oldfac <- fac.oldxsi
    iter <- 1

    while( ( iter <=msteps ) & ( parchange > convM)  ){
        Xlambda0 <- gammaslope <- Xlambda
        B <- tam_mml_3pl_computeB( Edes, gammaslope, E )

        # calculate probabilities
        res <- tam_mml_3pl_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi,
                    theta=theta, nnodes=nnodes, maxK=maxK, recalc=TRUE, guess=guess )
        rprobs <- res$rprobs
        rprobs0 <- res$rprobs0

        # init derivatives
        d2.b <- d1.b <- rep(eps,Nlam)
        res <- tam_rcpp_mml_3pl_slca_deriv(XdesM=FdesM, dimXdes=dimFdes, Xlambda=gammaslope,
                    probs=as.vector(rprobs), nik=as.vector(n.ik), Nik=as.vector(N.ik),
                    guess=guess, probs0=as.vector(rprobs0) )
        d1.b <- res$d1b
        d2.b <- res$d2b

        # prior distribution for gammaslope
        if ( ! is.null(gammaslope.prior) ){
            h <- 1E-4
            if ( ncol(gammaslope.prior)==2 ){
                d0  <- log( stats::dnorm( Xlambda, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2] ) + eps)
                d0p  <- log( stats::dnorm( Xlambda + h, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2] ) + eps)
                d0m  <- log( stats::dnorm( Xlambda - h, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2] ) + eps)
            }
            if ( ncol(gammaslope.prior)==4 ){
                d0  <- log( tam_dtnorm( Xlambda, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2], lower=gammaslope.prior[,3],
                                upper=gammaslope.prior[,4] ) + eps)
                d0p  <- log( tam_dtnorm( Xlambda + h, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2], lower=gammaslope.prior[,3],
                                upper=gammaslope.prior[,4] ) + eps)
                d0m  <- log( tam_dtnorm( Xlambda - h, mean=gammaslope.prior[,1],
                                sd=gammaslope.prior[,2], lower=gammaslope.prior[,3],
                                upper=gammaslope.prior[,4] ) + eps)
            }
            res <- tam_difference_quotient( d0=d0, d0p=d0p, d0m=d0m, h=h)
            d1 <- res$d1
            d2 <- res$d2
            #  d1 <- ( d0p - d0 ) / h
            #  d2 <- ( ( d0p - d0 ) - ( d0 - d0m ) ) / h^2
            d1.b <- d1.b + d1
            d2.b <- d2.b + d2
        }
        increment <-   d1.b / ( abs( d2.b + eps ) )
        increment[ is.na(increment) ] <- 0

        increment <- tam_trim_increment(increment=increment,
                            max.increment=max.increment, trim_increment="cut")

        max.increment <- max(abs(increment)) / .98
        Xlambda <- Xlambda + increment
        se.Xlambda <- sqrt( 1 / abs( d2.b+eps ) )

        Xlambda <- ifelse( Xlambda > maxgamma, maxgamma, Xlambda )
        Xlambda <- ifelse( Xlambda < - maxgamma, - maxgamma, Xlambda )
        if ( ! is.null(gammaslope.prior) ){
            if ( ncol(gammaslope.prior)==4 ){
                Xlambda <- ifelse( Xlambda < gammaslope.prior[,3],
                                        gammaslope.prior[,3] + 1.3* h, Xlambda )
                 Xlambda <- ifelse( Xlambda > gammaslope.prior[,4],
                                        gammaslope.prior[,4] - 1.3* h, Xlambda )
            }
        }

        if ( ! is.null( Xlambda.fixed) ){
            Xlambda[ Xlambda.fixed[,1] ] <- Xlambda.fixed[,2]
            se.Xlambda[ Xlambda.fixed[,1] ] <- 0
        }

        if (progress){ cat("-") ; utils::flush.console() }
        iter <- iter + 1
        parchange <- max( abs(Xlambda0-Xlambda))
    }
    #********* end algorithm

    if (oldfac > 0 ){
        Xlambda <- oldfac*Xlambda00 + ( 1 - oldfac ) *Xlambda
    }
    max.increment <- tam_parameter_change( Xlambda, Xlambda00 )
    gammaslope <- Xlambda

    #--- constrain gamma slope
    if ( ! is.null(gammaslope.constr.V) ){
        e1 <- matrix( gammaslope, ncol=1 )
        gammaslope <- ( e1 + V %*% V1 %*% ( e2 - t(V) %*% e1 ) )[,1]
    }

    #--- centering gammaslope
    gammaslope <- tam_mml_3pl_mstep_item_slopes_gammaslope_center( gammaslope=gammaslope,
                        gammaslope.center.index=gammaslope.center.index,
                        gammaslope.center.value=gammaslope.center.value )

    #-- user function gammaslope
    if ( ! is.null( userfct.gammaslope ) ){
        gammaslope <- do.call( userfct.gammaslope, list(gammaslope) )
    }
    gammaslope <- fac.oldxsi * gammaslope0 + ( 1 - fac.oldxsi)*gammaslope

    #--- gammaslope acceleration
    if ( gammaslope_acceleration$acceleration !="none" ){
        gammaslope_acceleration <- tam_accelerate_parameters( xsi_acceleration=gammaslope_acceleration,
                        xsi=gammaslope, iter=iter, itermin=3)
        gammaslope <- gammaslope_acceleration$parm
    }

    #--- parameter change
    gammaslope_change <- tam_parameter_change( gammaslope, gammaslope0)

    #--- recompute B
    B <- tam_mml_3pl_computeB( Edes=Edes, gammaslope=gammaslope, E=E )

    #---- OUTPUT
    res <- list("gammaslope"=Xlambda, "se.gammaslope"=se.Xlambda,
                 "max.increment.b"=max.increment,
                 "gammachange"=max( abs( Xlambda00 - Xlambda) ),
                 gammaslope_change=gammaslope_change,
                 gammaslope_acceleration=gammaslope_acceleration, B=B
                 )
    return(res)
}
#----------------------------------------------------------

.mml.3pl.est.slopes <- tam_mml_3pl_mstep_item_slopes


# cat(" +++ compute B") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
