## File Name: tam_mml_se_quick.R
## File Version: 0.44


tam_mml_se_quick <- function( tamobj, numdiff.parm=.001, item_pars=TRUE )
{
    h <- numdiff.parm
    B <- tamobj$B
    A <- tamobj$A
    Y <- tamobj$Y
    YSD <- tamobj$YSD
    nitems <- tamobj$nitems
    xsi <- ( tamobj$xsi )[,1]
    beta <- tamobj$beta
    variance <- tamobj$variance
    nstud <- tamobj$nstud
    AXsi <- tamobj$AXsi
    resp <- tamobj$resp
    ndim <- tamobj$ndim
    theta <- tamobj$theta
    maxK <- tamobj$maxK
    pweights <- tamobj$pweights
    snodes <- tamobj$control$snodes
    thetawidth <- diff(theta[,1] )
    thetawidth <- ( ( thetawidth[ thetawidth > 0 ])[1] )^ndim
    hwt <- tamobj$hwt
    resp <- tamobj$resp
    resp.ind <- tamobj$resp.ind
    resp.ind.list <- tamobj$resp.ind.list
    nnodes <- tamobj$nnodes
    ntheta <- length(theta)
    irtmodel <- tamobj$irtmodel
    est.slopegroups <- tamobj$est.slopegroups
    # multiplication parameters for numerical differentiation
    mult <- c(0,1,-1)

    ##############################################
    # Item parameters xsi
    ##############################################
    ip <- length(xsi)
    # create result object for item parameters
    se.xsi <- rep( 0, ip )
    if (item_pars){
        cat("Item parameters\n|")
        disp_progress <- tam_compute_disp_progress(ip=ip)
    }

    # prior distribution for each student (normal density)
    gwt0a <- tam_stud_prior( theta=theta, Y=Y, beta=beta, variance=variance,
                            nstud=nstud, nnodes=nnodes, ndim=ndim, YSD=YSD,
                            unidim_simplify=FALSE )

    ll <- matrix( 0, nrow=nstud, ncol=3 )
    vv <- 1

    if ( ! item_pars ){
        se.xsi <- NA*se.xsi
    } else {
        for (pp in 1:ip){
            vec <- tam_mml_se_quick_modify_parameter_vec(pp=pp)
            for (mm in vec){
                xsi0 <- xsi
                xsi0[pp] <- xsi0[pp] + mult[mm] * h
                #-- compute likelihood
                ll[,mm] <- tam_mml_se_quick_likelihood( nitems=nitems, A=A, AXsi=AXsi, B=B, xsi=xsi0,
                            theta=theta, nnodes=nnodes, maxK=maxK, gwt=gwt0a, resp=resp,
                            resp.ind.list=resp.ind.list, snodes=snodes, thetawidth=thetawidth,
                            thetasamp.density=tamobj$thetasamp.density )
            }
            info_pp <- tam_mml_se_quick_difference_quotient(ll=ll, h=h, pweights=pweights )
            se.xsi[pp] <- sqrt( - 1 / info_pp )
            #-- verbose
            vv <- tam_mml_se_quick_verbose(pp=pp, disp_progress=disp_progress, vv=vv )

        }
        cat("|\n")
    }

    ##############################################
    # Item parameters B
    ##############################################
    se.B <- 0*B
    get_se_B <- irtmodel %in% c("2PL", "GPCM", "GPCM.design", "2PL.groups")
    if ( ! item_pars ){
        get_se_B <- FALSE
        se.B <- NA*B
    }

    if( get_se_B  ){
        cat("Loading parameters\n|")
        if(irtmodel=="GPCM.design"){
            basispar <- tamobj$basispar
            E <- tamobj$E
            pair.ind <- which(lower.tri(diag(length(basispar)), TRUE), arr.ind=TRUE)
            se.basispar <- rep(0,length(basispar))
            warning("SE for discrimination in irtmodel='GPCM.design' is experimental.")
        }
        ip <- switch(irtmodel,
                    "2PL.groups"=length(unique(est.slopegroups)),
                    "GPCM.design"=length(basispar),
                    length(B[,-1,]) # default
        )
        # create result object for item parameters
        disp_progress <- tam_compute_disp_progress(ip=ip)

        ll <- matrix( 0, nrow=nstud, ncol=3 )
        vv <- 1
        for (pp in 1:ip){
            vec <- tam_mml_se_quick_modify_parameter_vec(pp=pp)
            pp.ind <- switch(irtmodel,
                        "2PL.groups"=which(est.slopegroups==sort(unique(est.slopegroups))[pp]),
                        #"GPCM.design"=pair.ind[pp,,drop=FALSE],
                        pp # default)
                        )
            for (mm in vec){
                B0 <- B
                if(irtmodel %in% c("2PL", "GPCM", "2PL.groups")){
                    B0[,-1,][ pp.ind ] <- B0[,-1,][ pp.ind ] + mult[mm] * h
                }
                if(irtmodel=="GPCM.design"){
                    basispar0 <- basispar
                    basispar0[ pp.ind ] <- basispar0[ pp.ind ] + mult[mm] * h
                    B0[,-1,] <- E %*% basispar0
                }
                #-- compute likelihood
                ll[,mm] <- tam_mml_se_quick_likelihood( nitems=nitems, A=A, AXsi=AXsi, B=B0, xsi=xsi,
                            theta=theta, nnodes=nnodes, maxK=maxK, gwt=gwt0a, resp=resp,
                            resp.ind.list=resp.ind.list, snodes=snodes, thetawidth=thetawidth )
            }
            info_pp <- tam_mml_se_quick_difference_quotient(ll=ll, h=h, pweights=pweights )
            if(irtmodel %in% c("2PL", "GPCM", "2PL.groups")){
                se.B[,-1,][pp.ind] <- sqrt( - 1 / info_pp )
            }
            if(irtmodel=="GPCM.design"){
                var.basispar[ pp.ind ] <- ( - 1 / info_pp )
            }
            vv <- tam_mml_se_quick_verbose(pp=pp, disp_progress=disp_progress, vv=vv )
        }

        if(irtmodel=="GPCM.design"){
            se.B[,-1,] <- sqrt( diag(( E %*% diag(var.basispar) %*% t(E) )) )
        }
        cat("|\n")
    }

    ##############################################
    # Regression parameters
    ##############################################
    # create result object for item parameters
    se.beta <- 0*beta
    nreg <- nrow(beta)
    cat("Regression parameters\n|")
    ip <- nreg*ndim
    disp_progress <- tam_compute_disp_progress(ip=ip)
    cat("|\n|")
    vv <- 1
    pp1 <- 1
    # compute response probabilities
    for (pp in 1:nreg){
        for (dd in 1:ndim){
            for (mm in 2:3){
                beta0 <- beta
                beta0[ pp,dd] <- beta0[pp,dd] + mult[mm] * h
                #--- prior distribution for each student (normal density)
                gwt0a <- tam_stud_prior( theta=theta, Y=Y, beta=beta0, variance=variance,
                                nstud=nstud, nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=FALSE )
                #-- compute likelihood
                ll[,mm] <- tam_mml_se_quick_likelihood( nitems=nitems, A=A, AXsi=AXsi, B=B, xsi=xsi,
                            theta=theta, nnodes=nnodes, maxK=maxK, gwt=gwt0a, resp=resp,
                            resp.ind.list=resp.ind.list, snodes=snodes, thetawidth=thetawidth,
                            thetasamp.density=tamobj$thetasamp.density  )
            }
            info_pp <- tam_mml_se_quick_difference_quotient(ll=ll, h=h, pweights=pweights )
            se.beta[pp,dd] <- sqrt( - 1 /  info_pp )
            vv <- tam_mml_se_quick_verbose(pp=pp, disp_progress=disp_progress, vv=vv )
            pp1 <- pp1+1 ;
        }
    }


    #-----------------------------------------------------------
    # handle fixed parameters
    if ( ! is.null( tamobj$xsi.fixed ) ){
        se.xsi[ tamobj$xsi.fixed[,1] ] <- 0
    }
    if ( ! is.null( tamobj$beta.fixed ) ){
        se.beta[ tamobj$beta.fixed[,1:2] ] <- 0
    }
    if ( ! is.null( tamobj$B.fixed ) ){
        se.B[ tamobj$B.fixed[,1:3] ] <- 0
    }
    if ( ! is.null( tamobj$Q ) & tamobj$ndim>1 ){
        Q.ind <- which(tamobj$Q==0, arr.ind=TRUE)
        Q.ind <- cbind(Q.ind[rep(1:nrow(Q.ind), maxK),], rep(1:maxK, each=nrow(Q.ind)))
        se.B[ Q.ind[,c(1,3,2)] ] <- 0
    }

    #-----------------------------------------------------------
    cat("|\n")


    #***
    N1 <- nrow(tamobj$item)
    if (N1 !=length(xsi) ){
        xsi <- data.frame( "item"=rownames(tamobj$xsi), "N"=NA,
                    "est"=xsi, "se"=se.xsi )
    } else {
        xsi <- data.frame( "item"=rownames(tamobj$xsi),
                    "est"=xsi, "se"=se.xsi )
    }

    beta <- data.frame( "beta"=beta, "se"=se.beta )
    colnames(beta) <- c( paste("est.Dim", 1:ndim, sep="")    , paste("se.Dim", 1:ndim, sep="")    )

    B.out <- data.frame( "item"=dimnames(B)[[1]] )
    for (kk in 1:(maxK-1)){ # kk <- 1
        for (dd in 1:ndim){
            B.out[, paste0("B.Cat", kk,".Dim",dd) ] <- B[,kk+1,dd]
            B.out[, paste0("se.B.Cat", kk,".Dim",dd) ] <- se.B[,kk+1,dd]
        }
    }

    utils::flush.console()
    res <- list( "xsi"=xsi, "beta"=beta, "B"=B.out )
    if(irtmodel=="GPCM.design"){
        basispar.res <- data.frame("basispar"=1:length(basispar),
                "gamma"=basispar, "se"=sqrt(var.basispar) )
        res <- c(res, list("basispar"=basispar.res))
    }
    #--- output
    return(res)
}
