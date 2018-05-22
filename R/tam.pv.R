## File Name: tam.pv.R
## File Version: 9.466
tam.pv <- function( tamobj, nplausible=10,
            ntheta=2000, normal.approx=FALSE, samp.regr=FALSE,
            theta.model=FALSE, np.adj=8, na.grid=5, verbose=TRUE)
{
    #####################################################
    # INPUT:
    # tamobj ... result from tam analysis
    # nplausible ... number of plausible values
    # ntheta ... number of simulated theta values
    # samp.regr ... sample regression coefficients?
    #        when sampling regression coefficients,
    #        plausible values are used for recalculating
    #        regression coefficients
    #        (sampling of regression coefficients only
    #          works in the unidimensional case)
    # normal.approx ... use normal distribution as an
    #                    approximation of the posterior
    ####################################################

    a0 <- Sys.time()
    type <- "nonparm"        # there is no type='normal' up to now implemented
    latreg <- FALSE

    #-- check for recommendation of tam.pv.mcmc
    res <- tam_pv_recommend_tam_pv_mcmc(tamobj=tamobj)

    if ( class(tamobj)=="tam.latreg" ){
        theta.model <- TRUE
        latreg <- TRUE
        like <- tamobj$like
    }
    if ( ! latreg ){
        if (class(tamobj)!="tam.mml.3pl"){
            guess <- rep( 0, dim(tamobj$B)[1] )
        } else {
            guess <- tamobj$guess
        }
        B <- tamobj$B
        A <- tamobj$A
        AXsi <- tamobj$AXsi
        xsi <- ( tamobj$xsi )[,1]
        maxK <- tamobj$maxK
    }
    Y <- tamobj$Y
    YSD <- tamobj$YSD
    nitems <- tamobj$nitems
    snodes <- tamobj$control$snodes
    ndim <- tamobj$ndim
    beta <- tamobj$beta
    variance <- tamobj$variance
    nstud <- tamobj$nstud

    if ( theta.model | ( normal.approx & (ndim > 1) ) ){
        ntheta <- nrow(tamobj$theta)
    }

    nthetal <- rep( 1, ntheta )
    nnodes <- ntheta
    ndim <- tamobj$ndim
    pweights <- tamobj$pweights

    #***************************
    # define theta grid
    #--- dim=1
    mu1 <- NULL
    Sigma1 <- NULL
    if ( ndim==1 ){
        MEAP <- mean( tamobj$person$EAP )
        SDEAP <- sqrt( stats::var( tamobj$person$EAP ) + mean( tamobj$person$SD.EAP^2 ) )
    }
    #--- dim > 1
    if ( ndim > 1 ){
        tp1 <- tamobj$person
        ind <- grep("EAP\\.Dim", colnames(tp1) )
        ind <- ind[ seq( 1, length(ind), 2 ) ]
        dat1 <- tp1[,  ind ]
        mu1 <- as.vector( colMeans( dat1 ) )
        var1 <- apply( dat1, 2, stats::var ) / tamobj$EAP.rel
        Sigma1 <- stats::cov2cor(variance)
        Sigma1 <- np.adj * diag( sqrt( var1) ) %*% Sigma1 %*% diag( sqrt( var1 ))
    }

    # create pv matrix (uni- and multidimensional case)
    pv <- matrix( 0, nrow=nstud, ncol=nplausible*ndim)
    NPV <- nplausible
    pp <- 1
    iter <- 1
    iterate <- TRUE
    if ( verbose ){
        cat("|")
        cat( paste( rep("*", nplausible ), collapse="") )
        cat("|\n|")
        utils::flush.console()
    }

    ###################################################
    # routine for drawing plausible values
    while ( iterate ){
        #--- sampling theta
        res <- tam_pv_sampling_theta( theta.model=theta.model, ndim=ndim, normal.approx=normal.approx,
                    tamobj=tamobj, MEAP=MEAP, SDEAP=SDEAP, np.adj=np.adj, theta=theta, ntheta=ntheta,
                    mu1=mu1, Sigma1=Sigma1, na.grid=na.grid )
        theta <- res$theta

        #--- compute item response probabilities
        if ( ! latreg ){
            res <- tam_mml_3pl_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi, theta=theta,
                            nnodes=nnodes, maxK=maxK, recalc=TRUE, guess=guess)
            rprobs <- res$rprobs
            AXsi <- res$AXsi
        }

        #--- calculate student prior distribution
        gwt <- tam_stud_prior( theta=theta, Y=Y, beta=beta, variance=variance, nstud=nstud,
                        nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=FALSE,
                        snodes=snodes )
        ind0 <- which( rowSums(gwt)==0 )
        if ( length(ind0) > 0 ){
            gwt[ind0,] <- 1
        }

        #--- posterior distribution
        if ( ! latreg ){
            hwt <- tam_calc_posterior( rprobs=rprobs, gwt=gwt, resp=tamobj$resp, nitems=nitems,
                            resp.ind.list=tamobj$resp.ind.list, normalization=TRUE,
                            thetasamp.density=NULL, snodes=0 )$hwt
        }

        if (latreg){
            hwt <- like * gwt
            hwt <- hwt / rowSums(hwt)
        }
        hwt1 <- hwt

        #--- cumulative posterior probabilities
        hwt1 <- tam_rowCumsums(matr=hwt1)


        #**** sampling of regression coefficients
        if ( samp.regr ){
            #-- no normal approximation
            if ( ! normal.approx){
                res <- tam_pv_draw_pv_nonparametric( nstud=nstud, hwt1=hwt1, theta=theta, pv=pv,
                            ndim=ndim, pp=pp )
            }
            #-- normal approximation in unidimensional case
            if ( normal.approx & ( ndim==1 ) ){
                res <- tam_pv_draw_pv_normal_approximation_1dim( theta=theta, nstud=nstud,
                            ntheta=ntheta, pv=pv, hwt=hwt, pp=pp )
            }

            if ( normal.approx & ( ndim > 1) ){
                res <- tam_pv_draw_pv_normal_approximation_multidim( theta=theta, hwt=hwt,
                                    pp=pp, ndim=ndim, pv=pv )
            }
            pv <- res$pv
            theta1 <- res$theta1

            pp <- pp + 1
            if (iter==1){
                pp <- pp - 1
            }
            iter <- iter + 1
            if (pp > NPV){
                iterate <- FALSE
            }

            #-- sample beta value
            beta <- tam_pv_sampling_beta( theta1=theta1, ndim=ndim, Y=Y, pweights=pweights )

            if (iter>2){
                if (verbose){
                    cat("-" )
                    utils::flush.console()
                }
            }

        }

        #**** no sampling of regression cofficients
        if ( ! samp.regr ){
            for ( pp in 1:nplausible ){
                #-- no normal approximation
                if (  ! normal.approx  ){
                    res <- tam_pv_draw_pv_nonparametric( nstud=nstud, hwt1=hwt1, theta=theta, pv=pv,
                                ndim=ndim, pp=pp )
                }
                #-- normal approximation
                if ( normal.approx & ( ndim==1) ){
                    res <- tam_pv_draw_pv_normal_approximation_1dim( theta=theta, nstud=nstud,
                            ntheta=ntheta, pv=pv, hwt=hwt, pp=pp )
                }
                if ( normal.approx & ( ndim > 1) ){
                    res <- tam_pv_draw_pv_normal_approximation_multidim( theta=theta, hwt=hwt,
                                    pp=pp, ndim=ndim, pv=pv )
                }
                pv <- res$pv
                if (verbose){
                    cat("-")
                    utils::flush.console()
                }
            }
            NPV <- nplausible / 2
            iterate <- FALSE
        }   # end no plausible
        #--------------------------
        utils::flush.console()
    }  # end while loop
    ##################################################
    if (verbose){ cat("|\n") }
    #-- label the pv matrix
    colnames(pv) <- paste("PV", rep(1:nplausible,each=ndim),
                    ".Dim", rep(1:ndim,nplausible), sep="")
    pv <- data.frame( "pid"=tamobj$pid, pv )
    res <- list( pv=pv, hwt=hwt, hwt1=hwt1,
                theta=theta, ndim=ndim, nplausible=nplausible,
                pid=tamobj$pid, pweights=tamobj$pweights )
    class(res) <- "tam.pv"
    return(res)
}
##################################################################
