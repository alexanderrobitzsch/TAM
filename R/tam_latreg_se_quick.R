## File Name: tam_latreg_se_quick.R
## File Version: 0.09


tam_latreg_se_quick <- function( tamobj , numdiff.parm = .001)
{
    h <- numdiff.parm
    Y <- tamobj$Y
    YSD <- tamobj$YSD
    nitems <- tamobj$nitems
    xsi <- ( tamobj$xsi )[,1]
    beta <- tamobj$beta
    variance <- tamobj$variance
    nstud <- tamobj$nstud
    ndim <- tamobj$ndim
    theta <- tamobj$theta
    pweights <- tamobj$pweights
    snodes <- tamobj$control$snodes
    thetawidth <- diff(theta[,1] )
    thetawidth <- ( ( thetawidth[ thetawidth > 0 ])[1] )^ndim
    hwt <- tamobj$hwt
    nnodes <- tamobj$nnodes
    ntheta <- length(theta)
    like <- tamobj$like
    # multiplication parameters for numerical differentiation
    ll <- matrix( 0, nrow=nstud , ncol=3 )
    mult <- c(0,1,-1)


    ##############################################
    # Regression parameters
    ##############################################
    # create result object for item parameters
    se.beta <- 0*beta
    nreg <- nrow(beta)
    cat("Regression parameters\n|")
    ip <- nreg*ndim
    disp_progress <- tam_compute_disp_progress(ip=ip)
    # cat("|\n|")

    beta0 <- beta
    #--- prior distribution for each student (normal density)
    gwt0a <- tam_stud_prior( theta=theta , Y=Y , beta=beta0 , variance=variance ,
                                nstud=nstud , nnodes=nnodes , ndim=ndim, YSD=YSD, unidim_simplify=FALSE )
    #-- compute likelihood
    ll[,1] <- tam_latreg_se_quick_likelihood( gwt=gwt0a, like=like,
                                thetawidth=thetawidth, snodes=snodes )
    vv <- 1
    pp1 <- 1
    # compute response probabilities
    for (pp in 1:nreg){
        for (dd in 1:ndim){
            for (mm in 2:3){
                beta0 <- beta
                beta0[ pp ,dd] <- beta0[pp,dd] + mult[mm] * h
                #--- prior distribution for each student (normal density)
                gwt0a <- tam_stud_prior( theta=theta , Y=Y , beta=beta0 , variance=variance ,
                                nstud=nstud , nnodes=nnodes , ndim=ndim, YSD=YSD, unidim_simplify=FALSE )
                #-- compute likelihood
                ll[,mm] <- tam_latreg_se_quick_likelihood( gwt=gwt0a, like=like,
                                thetawidth=thetawidth, snodes=snodes )
            }
            info_pp <- tam_mml_se_quick_difference_quotient(ll=ll, h=h, pweights=pweights )
            se.beta[pp,dd] <- sqrt( - 1 /  info_pp )
            vv <- tam_mml_se_quick_verbose(pp=pp, disp_progress=disp_progress, vv=vv )
            pp1 <- pp1+1 ;
        }
    }

    #-----------------------------------------------------------
    cat("|\n")

    beta <- data.frame( "beta" = beta , "se" = se.beta )
    colnames(beta) <- c( paste("est.Dim" , 1:ndim , sep="")    , paste("se.Dim" , 1:ndim , sep="")    )

    utils::flush.console()
    res <- list( "beta" = beta )
    #--- output
    return(res)
}
