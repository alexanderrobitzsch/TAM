## File Name: tam_mml_3pl_mstep_item_guessing.R
## File Version: 9.410
####################################################
# estimation guessing parameters
tam_mml_3pl_mstep_item_guessing <- function( guess, Msteps, convM,
        nitems, A, AXsi, B, xsi, theta, nnodes, maxK,
        n.ik, N.ik, est.guess, old.increment.guess, guess.prior,
        progress, max.guess, guess_acceleration, iter )
{
    old_increment <- old.increment.guess
    if (progress){
        cat("\nM Step Guessing     |")
        utils::flush.console()
    }
    oldguess <- guess
    eps <- 1e-10
    eps10 <- 1E-30
    guess.logit0 <- guess.logit <- stats::qlogis( guess + eps)
    ind.guess <- which( est.guess !=0 )
    n1ij <- n.ik[,2,]
    nij <- N.ik
    Miter <- 1
    converge <- FALSE
    guess_old <- guess
    h <- 1E-4

    while ( !converge & ( Miter <=Msteps ) ){
        calc_prob_args <- list( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi, theta=theta,
                                    nnodes=nnodes, maxK=maxK, guess=guess )
        res.p <- do.call( "tam_mml_3pl_calc_prob", calc_prob_args )
        rprobs00 <- rprobs <- res.p$rprobs
        rprobs0 <- res.p$rprobs0

        pij <- rprobs[, 2, ]
        pij <- pij + eps
        pij0 <- rprobs0[, 2, ]
        guess <- ifelse( ( guess < h ) & ( est.guess !=0 ), 4*h, guess )
        #--- derivatives guessing priors
        if ( ! is.null(guess.prior) ){
            d0  <- log( stats::dbeta( guess, guess.prior[,1], guess.prior[,2] ) + eps)
            d0p <- log( stats::dbeta( guess + h, guess.prior[,1], guess.prior[,2] ) + eps)
            d0m <- log( stats::dbeta( guess - h, guess.prior[,1], guess.prior[,2] ) + eps)
            res <- tam_difference_quotient( d0=d0, d0p=d0p, d0m=d0m, h=h)
            d1 <- res$d1
            d2 <- res$d2
        }

        #-- probabilities evaluated at guess+h and guess-h
        # calc_prob_args1 <- calc_prob_args
        #-    LL( guess + h)
        calc_prob_args1 <- tam_args_replace_value( args=calc_prob_args,
                                variable="guess", value=guess+h)
        rprobs1 <- do.call( "tam_mml_3pl_calc_prob", calc_prob_args1 )$rprobs
        #-  LL( guess - h)
        calc_prob_args1 <- tam_args_replace_value( args=calc_prob_args,
                                variable="guess", value=guess-h)
        rprobs2 <- do.call( "tam_mml_3pl_calc_prob", calc_prob_args1 )$rprobs
        #*** calculate log-likelihood
        n0ij <- nij - n1ij
        eps2 <- 1E-5
        l0 <- tam_mml_3pl_calc_ll_est_guessing( n0ij, n1ij, probs=rprobs00, eps=eps2 )
        l1p <- tam_mml_3pl_calc_ll_est_guessing( n0ij, n1ij, probs=rprobs1, eps=eps2 )
        l1m <- tam_mml_3pl_calc_ll_est_guessing( n0ij, n1ij, probs=rprobs2, eps=eps2 )
        res <- tam_difference_quotient( d0=l0, d0p=l1p, d0m=l1m, h=h)
        der1 <- res$d1
        der2 <- res$d2

        #***
        # first derivative
        if ( ! is.null( guess.prior) ){
            der1 <- der1 + d1
            der2 <- der2 + d2
        }

        # aggregation over group of parameters
        der1 <- tam_aggregate(der1, est.guess)
        der2 <- tam_aggregate(der2, est.guess)
        der1 <- der1[ der1[,1] !=0,, drop=FALSE]
        der2 <- der2[ der2[,1] !=0,, drop=FALSE]
        increment0 <- increment <- der1[,2] / ( abs(der2[,2]) + eps )

        increment <- tam_trim_increment(increment=increment,
                            max.increment=abs(old_increment), trim_increment="half")

        increment <- increment[ est.guess ]
        guess[ ind.guess ] <- guess[ind.guess ] + increment
        guess <- ifelse( ( guess < h ) & ( est.guess !=0 ), 4*h, guess )
        guess <- ifelse( guess > max.guess, max.guess, guess )
        old_increment <- max(abs(increment))
        if ( old_increment < convM){
            converge <- TRUE
        }
        Miter <- Miter + 1
        if (progress){
            cat("-")
            utils::flush.console()
        }
    }
    #*********************************************************
    # standard error of logit guessing parameter
    # se.guess <- sqrt( 1 / abs(der2[ est.guess, 2] ) )

    guess.change <- max( abs( guess - guess_old ))
    se.guess <-  sqrt( 1 / ( abs( der2[ est.guess,2] ) + eps10 ) )
    se2 <- 0*guess
    se2[ ind.guess ] <- se.guess[ est.guess ]

    #--- acceleration
    if ( guess_acceleration$acceleration !="none" ){
        g1 <- guess
        guess_acceleration <- tam_accelerate_parameters( xsi_acceleration=guess_acceleration,
                        xsi=g1, iter=iter, itermin=3,
                        ind=guess_acceleration$ind_guess)
        guess <- guess_acceleration$parm
        guess[ guess < 0 ] <- 1E-5
        guess.change <- max( abs(guess - oldguess))
    }

    # transform standard errors according to delta formula
    # h=plogis=( 1 + exp( -x ) )^(-1)
    # h'=-1 * exp(-x) * ( 1 + exp( -x ) )^(-2)    =h * ( 1 - h )
    # hast <- guess * ( 1 - guess )
    # se2 <- sqrt( hast^2 ) * se2
    res <- list( "guess"=guess, "guess.change"=guess.change,
            se.guess=se2, guess_acceleration=guess_acceleration )
    return(res)
}
################################################################################

.mml.3pl.est.guessing <- tam_mml_3pl_mstep_item_guessing
