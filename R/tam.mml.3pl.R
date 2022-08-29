## File Name: tam.mml.3pl.R
## File Version: 9.888

tam.mml.3pl <- function( resp, Y=NULL, group=NULL,
            formulaY=NULL, dataY=NULL,
            ndim=1, pid=NULL,
            xsi.fixed=NULL,  xsi.inits=NULL, xsi.prior=NULL,
            beta.fixed=NULL, beta.inits=NULL,
            variance.fixed=NULL, variance.inits=NULL,
            est.variance=TRUE,
            A=NULL, notA=FALSE, Q=NULL,
            Q.fixed=NULL,
            E=NULL, gammaslope.des="2PL",
            gammaslope=NULL, gammaslope.fixed=NULL,
            est.some.slopes=TRUE, gammaslope.max=9.99,
            gammaslope.constr.V=NULL, gammaslope.constr.c=NULL,
            gammaslope.center.index=NULL,  gammaslope.center.value=NULL,
            gammaslope.prior=NULL,  userfct.gammaslope=NULL,
            gammaslope.constr.Npars=0,
            est.guess=NULL,  guess=rep(0,ncol(resp)),
            guess.prior=NULL, max.guess=.50,
            skillspace="normal", theta.k=NULL,
            delta.designmatrix=NULL, delta.fixed=NULL,
            delta.inits=NULL,  pweights=NULL,
            item.elim=TRUE, verbose=TRUE,
            control=list(),    Edes=NULL
                )
{

    s1 <- Sys.time()
    CALL <- match.call()

    # display
    disp <- "....................................................\n"
    increment.factor <- progress <- nodes <- snodes <- ridge <- xsi.start0 <- QMC <- NULL
    maxiter <- conv <- convD <- min.variance <- max.increment <- Msteps <- convM <- NULL
    delta <- R <- NULL
    B <- NULL ; B.fixed <- NULL ; theta <- NULL
    fac.oldxsi <- acceleration <- NULL
    irtmodel <- "2PL"
    est.slopegroups <- NULL
    init.gammaslope <- ( ! is.null( gammaslope ) )
    maxgamma <- gammaslope.max

    #**** handle verbose argument
    args_CALL <- as.list( sys.call() )
    if ( ! tam_in_names_list( list=control, variable="progress" ) ){
        control$progress <- verbose
    }
    #*******

    resp <- as.matrix(resp)
    resp0 <- resp <- add.colnames.resp(resp)

    #--- create E design matrix from different input matrices
    E_null <- is.null(E)
    res0 <- tam_mml_3pl_create_E( resp=resp, E=E, Q=Q,
                    gammaslope.des=gammaslope.des, Q.fixed=Q.fixed )
    E <- res0$E
    if ( is.null(gammaslope.fixed ) ){
        gammaslope.fixed <- res0$gammaslope.fixed
    }

    # calculation of not A if requested
    if ( notA) {
        res <- tam_mml_3pl_create_notA( E, notA )
        A <- res$A
        xsi.fixed <- res$xsi.fixed
    }

    #********************
    #********************
    # compute B from E or an input statement
     # starting values gammaslope
    if ( is.null( gammaslope ) ){
        Ngam <- dim(E)[4]
        if ( est.some.slopes){
            gammaslope <- stats::runif( Ngam, .9, 1.1 )
        } else {
            gammaslope <- rep(1,Ngam )
        }
        }
        if ( ! is.null( gammaslope.fixed ) ){
            gammaslope[ gammaslope.fixed[,1] ] <- gammaslope.fixed[,2]
    }
    if ( is.null(Edes) ){
        Edes <- tam_rcpp_mml_3pl_nonzero_entries( E=as.vector(E), dimE=dim(E) )$E_design
    }
    B <- tam_mml_3pl_computeB( Edes=Edes, gammaslope=gammaslope, E=E )


    #***********************
    if ( is.null(A)){ printxsi <- FALSE  } else { printxsi <- TRUE }

    #*******
      #--- attach control elements
    e1 <- environment()
    tam_fct <- "tam.mml.3pl"
    res <- tam_mml_control_list_define(control=control, envir=e1, tam_fct=tam_fct)
    con <- res$con
    con1a <- res$con1a

    if (progress){
      cat(disp)
      cat("Processing Data     ", paste(Sys.time()), "\n") ; flush.console()
    }

    #-- activate stochastic/QMC integration in case of multiple groups
    #-- if requested
    # if ( ! is.null(group) ){
    if (FALSE){
      con1a$QMC <- QMC <- FALSE
      con1a$snodes <- snodes <- 0
    }
    if ( is.null(group) ){
      group1 <- rep(1,nrow(resp) )
    }

    # define design matrix in case of PCM2
    if (( irtmodel=="PCM2" ) & (is.null(Q)) & ( is.null(A)) ){
      A <- .A.PCM2( resp )
    }

    if ( ! is.null( variance.fixed ) ){
        est.variance <- TRUE
    }

    # manage guessing parameters
    if ( is.null(guess) ){
       guess <- rep( 0, ncol(resp) )
    }


    if ( ! is.null(est.guess) ){
        h1 <- setdiff( unique(est.guess), 0 )
        est.guess <- match( est.guess,h1 )
        est.guess[ is.na( est.guess ) ] <- 0
    }
    est.some.guess <- sum( est.guess > 0 )

    nitems <- ncol(resp)       # number of items
    nstud <- nrow(resp)        # number of students

    #*****
    nstud100 <- sum(1*( rowSums( 1 - is.na(resp) ) > 0 ))


    if ( is.null( pweights) ){
      pweights <- rep(1,nstud) # weights of response pattern
    }

    if (progress){
        cat("    * Response Data:", nstud, "Persons and ", nitems, "Items \n" )  ;
        flush.console()
    }

    #!! check dim of person ID pid
    if ( is.null(pid) ){ pid <- seq(1,nstud) }

    # normalize person weights to sum up to nstud
    pweights0 <- pweights
    pweights <- nstud * pweights / sum(pweights)
    # a matrix version of person weights
    pweightsM <- outer( pweights, rep(1,nitems) )

    # calculate ndim if only B or Q are supplied
    if ( ! is.null(B) ){ ndim <- dim(B)[3] }
    if ( ! is.null(Q) ){ ndim <- dim(Q)[2] }

    betaConv <- FALSE         #flag of regression coefficient convergence
    varConv <- FALSE          #flag of variance convergence
    nnodes <- length(nodes)^ndim
    if ( snodes > 0 ){ nnodes <- snodes }

    #--- print information about nodes
    res <- tam_mml_progress_proc_nodes( progress=progress, snodes=snodes,
                    nnodes=nnodes, skillspace=skillspace, QMC=QMC )

    # maximum no. of categories per item. Assuming dichotomous
    maxK <- max( resp, na.rm=TRUE ) + 1

    #****
    # ARb 2015-12-15
    maxKi <- NULL
    if ( ! (item.elim ) ){
        maxKi <- rep( maxK - 1, ncol(resp) )
    }
    #***

    # create design matrices
    design <- designMatrices( modeltype="PCM", maxKi=NULL, resp=resp,
                              A=A, B=B, Q=Q, R=R, ndim=ndim )
    A <- design$A
    B <- design$B
    cA <- design$flatA
    cA[is.na(cA)] <- 0
    if (progress){
        cat("    * Created Design Matrices   (", paste(Sys.time()), ")\n") ;
        utils::flush.console()
    }
    design <- NULL
    #---2PL---
    B_orig <- B  #keep a record of generated B before estimating it in 2PL model
    #---end 2PL---

    #--- xsi parameter index
    res <- tam_mml_proc_est_xsi_index(A, xsi.inits, xsi.fixed)
    np <- res$np
    xsi <- res$xsi
    est.xsi.index0 <- est.xsi.index <- res$est.xsi.index

    #--- inits group
    res <- tam_mml_inits_groups( group=group )
    G <- res$G
    groups <- res$groups
    group <- res$group
    var.indices <- res$var.indices

    #****************************
    # variance inits
    # initialise conditional variance
    res <- tam_mml_3pl_variance_fixed( variance=variance, variance.inits=variance.inits,
                G=G, ndim=ndim, variance.fixed=variance.fixed, est.variance=est.variance)
    variance.fixed <- res$variance.fixed
    variance <- res$variance

    #--- inits groups
    res <- tam_mml_3pl_inits_group(group=group, ndim=ndim, G=G, variance.inits=variance.inits,
                groups=groups)
    G <- res$G
    groups <- res$groups
    group <- res$group
    var.indices <- res$var.indices

    #--- inits beta regression coefficients
    res <- tam_mml_inits_beta( Y=Y, formulaY=formulaY, dataY=dataY, G=G, group=group,
                groups=groups, nstud=nstud, pweights=pweights, ridge=ridge, beta.fixed=beta.fixed,
                xsi.fixed=xsi.fixed, constraint="cases", ndim=ndim, beta.inits=beta.inits )
    Y <- res$Y
    nullY <- res$nullY
    formulaY <- res$formulaY
    nreg <- res$nreg
    W <- res$W
    YYinv <- res$YYinv
    beta.fixed <- res$beta.fixed
    beta <- res$beta

    #--- response indicators
    res <- tam_mml_proc_response_indicators( resp=resp, nitems=nitems )
    resp <- res$resp
    resp.ind <- res$resp.ind
    resp.ind.list <- res$resp.ind.list
    nomiss <- res$nomiss

    #-- AXsi
    AXsi <- matrix(0,nrow=nitems,ncol=maxK )  #A times xsi

    #--- parameter indices xsi parameters
    res <- tam_mml_proc_xsi_parameter_index_A(A=A, np=np)
    indexIP <- res$indexIP
    indexIP.list <- res$indexIP.list
    indexIP.list2 <- res$indexIP.list2
    indexIP.no <- res$indexIP.no

    #--- sufficient statistics for item parameters
    res <- tam_mml_sufficient_statistics( nitems=nitems, maxK=maxK, resp=resp, resp.ind=resp.ind,
                pweights=pweights, cA=cA, progress=progress )
    ItemScore <- res$ItemScore
    cResp <- res$cResp
    col.index <- res$col.index

    #--- inits xsi
    res <- tam_mml_inits_xsi( A=A, resp.ind=resp.ind, ItemScore=ItemScore, xsi.inits=xsi.inits,
                xsi.fixed=xsi.fixed, est.xsi.index=est.xsi.index, pweights=pweights,
                xsi.start0=xsi.start0, xsi=xsi, resp=resp )
    xsi <- res$xsi
    personMaxA <- res$personMaxA
    ItemMax <- res$ItemMax
    equal.categ <- res$equal.categ

    xsi.min.deviance <- xsi
    beta.min.deviance <- beta
    variance.min.deviance <- variance

    #--- create grid of nodes for numeric or stochastic integration
    res <- tam_mml_create_nodes( snodes=snodes, nodes=nodes, ndim=ndim, QMC=QMC,
                skillspace=skillspace, theta.k=theta.k)
    theta <- res$theta
    theta2 <- res$theta2
    thetawidth <- res$thetawidth
    theta0.samp <- res$theta0.samp
    thetasamp.density <- res$thetasamp.density
    nnodes <- res$nnodes
    snodes <- res$snodes
    ntheta <- res$ntheta
    deviance <- 0
    deviance.history <- tam_deviance_history_init(maxiter=maxiter)

    iter <- 0
    a02 <- a1 <- 999    # item parameter change
    #---2PL---
    # a4 <- 999
    basispar <- NULL
    #        } else{  a4 <- 0 }

    #*************************
    # skill space
    if ( ! is.null(theta) ){
        ntheta <- nrow(theta)
    }
    fulldesign <- FALSE
    if ( skillspace !="normal" ){
        gwt <- hwt <- matrix( 1/ntheta, nrow=nstud, ncol=ntheta)
        covdelta <- group1.list <- list(1:G)
        Ngroup <- rep(0,G)
        for (gg in 1:G){
            ind.gg <- which( group1==gg )
            Ngroup[gg] <- sum( pweights[ind.gg] )
            group1.list[[gg]] <- ind.gg
        }
        pi.k <- matrix( 1/ntheta, nrow=ntheta, ncol=G)
        if ( ! is.null( delta.designmatrix) ){
            if ( ncol(delta.designmatrix)==ntheta ){
                fulldesign <- TRUE
            }
        }

        # design matrix
        if ( is.null( delta.designmatrix) ){
            delta.designmatrix <- diag(ntheta )
            fulldesign <- TRUE
        }
        delta <- matrix( 0, nrow=ncol(delta.designmatrix), ncol=G)
    }
    gwt1 <- matrix( 1, nrow=nstud, ncol=ntheta )

    #***** inits for delta
    if ( ! is.null(delta.inits) ){
        delta <- delta.inits
    }

    #****** indicator matrices
    datindw <- list(1:maxK)
    for (kk in 1:maxK){
        datindw[[kk]] <- (resp==kk - 1 ) * resp.ind * pweights
    }

    #*************
    # gammaslope constraints
    e2 <- NULL
    V1 <- NULL
    if ( ! is.null(gammaslope.constr.V) ){
            V <- gammaslope.constr.V
            e2 <- matrix( gammaslope.constr.c, nrow=ncol(V), ncol=1 )
            V1 <- solve( crossprod(V) )
    }

      gammaslope <- .mml.3pl.gammaslope.center( gammaslope, gammaslope.center.index,
                         gammaslope.center.value  )


    #******
    # prior distribution guessing parameter
    if ( ! is.null(guess.prior) ){
        guess.mean <- guess.prior[,1] / rowSums( guess.prior )
        i1 <- which( guess.prior[,1] > 0 )
        guess[ i1 ] <- guess.mean[i1]
        guess.prior[ guess.prior==0 ] <- 1E-3
    }

    #---- prior distribution slope parameter
    if ( ( ! is.null(gammaslope.prior) ) & ( ! init.gammaslope) ){
        i1 <- which( gammaslope.prior[,2] < 10 )
        gammaslope[ i1 ] <- gammaslope.prior[i1,1]
    }

    #******
    # prior distribution slope parameter
    if ( ! is.null(xsi.prior) ){
        i1 <- which( xsi.prior[,2] < 10 )
        xsi[ i1 ] <- xsi.prior[i1,1]
    }

    #******
    # compute F design matrix for loadings
    Fdes <- tam_mml_3pl_compute_Fdes( E, gammaslope, theta )
    # use simplified design for F
    dimFdes <- dim(Fdes)
    res <- tam_rcpp_mml_3pl_calc_Fdes( XDES=as.vector(Fdes), dimXdes=dimFdes )
    FdesM <- res$FdesM[ 1:res$NFdesM, ]

    # **** AXsi parameters
    iIndex <- 1:dim(A)[1]
    LI <- length(iIndex)
    LXsi <- dim(A)[3]
    AXsi.tmp <- array( 0, dim=c( LI, maxK, nnodes ) )
    for (kk in 1:maxK){
        A_kk <- matrix( A[ iIndex, kk, ], nrow=LI, ncol=LXsi )
        AXsi.tmp[, kk, 1:nnodes ] <- A_kk %*% xsi
    }
    AXsi[iIndex,] <- AXsi.tmp[,,1]

    #--- compute B matrix
    B <- tam_mml_3pl_computeB( Edes=Edes, gammaslope=gammaslope, E=E, B=B,
                    skip_B=FALSE)

    ##**SE
    se.xsi <- 0*xsi
    se.B <- 0*B

    #--- create unidim_simplify
    res <- tam_mml_proc_unidim_simplify( Y=Y, A=A, G=G, beta.fixed=beta.fixed )
    unidim_simplify <- res$unidim_simplify
    YSD <- res$YSD
    Avector <- res$Avector

    #--- acceleration
    res <- tam_acceleration_inits(acceleration=acceleration, G=G, xsi=xsi,
                variance=variance, gammaslope=gammaslope, guess=guess,
                ind.guess=which( est.guess > 0 ), delta=delta )
    xsi_acceleration <- res$xsi_acceleration
    variance_acceleration <- res$variance_acceleration
    gammaslope_acceleration <- res$gammaslope_acceleration
    guess_acceleration <- res$guess_acceleration
    delta_acceleration <- res$delta_acceleration

    #************************
    # group indices definition
    group_indices <- as.list(1:G)
    for (gg in 1:G){
        group_indices[[gg]] <- which( group==gg )
    }
    #**************************

    hwt.min <- 0
    rprobs.min <- 0
    AXsi.min <- 0
    B.min <- 0
    deviance.min <- 1E100
    itemwt.min <- 0
    se.xsi.min <- se.xsi
    se.B.min <- se.B
    gammaslope_change <- gammaslope.change <- guess.change <- 0
    variance_change <- beta_change <- delta_change <- 0
    a4 <- 0
    a44 <- 1000
    old.increment.guess <- .6
    se.gammaslope <- NULL
    se.guess <- 0*guess
    no_stochastic_nodes <- snodes==0


    # display
    disp <- "....................................................\n"
    # define progress bar for M step
    mpr <- round( seq( 1, np, len=10 ) )

    if ( ! is.null(gammaslope.fixed ) ){
        gammaslope.fixed <- as.matrix(gammaslope.fixed)
    }

    # inits delta parameters
    if ( ! is.null( delta.inits) ){
        for ( gg in 1:G){
            pi.k[,gg] <- exp( delta.designmatrix %*% delta.inits[,gg] )
        }
    }

    #--- speed gains, further auxiliary objects, 2015-06-26
    unidim_simplify <- TRUE
    if (G > 1){ unidim_simplify <- FALSE }
    if (YSD){ unidim_simplify <- FALSE }

    ##############################################################
    ##############################################################
    ##############################################################
    # EM loop starts here
    while ( ( (!betaConv | !varConv)  | ((a1 > conv) | (a44 > conv) | (a02 > convD)) )  & (iter < maxiter) ) {
 a0 <- Sys.time()
        delta0 <- delta
        iter <- iter + 1
        #--- progress
        res <- tam_mml_progress_em0(progress=progress, iter=iter, disp=disp)
        #--- calculate nodes for Monte Carlo integration
        if ( snodes > 0){
            res <- tam_mml_update_stochastic_nodes( theta0.samp=theta0.samp, variance=variance,
                        snodes=snodes, beta=beta, theta=theta )
            theta <- res$theta
            theta2 <- res$theta2
            thetasamp.density <- res$thetasamp.density
        }
        olddeviance <- deviance

        #--- calculation of probabilities
        res <- tam_mml_3pl_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi,
                    theta=theta, nnodes=nnodes, maxK=maxK, recalc=TRUE, guess=guess )
        rprobs <- res$rprobs
        rprobs0 <- res$rprobs0
        AXsi <- res$AXsi

# cat("calc_prob") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #***********************************
        # student's prior distribution

        #--- normal distribution
        if (skillspace=="normal" ){
            gwt <- tam_stud_prior_multiple_groups( theta=theta, Y=Y, beta=beta,
                        variance=variance, nstud=nstud, G=G, group_indices=group_indices,
                        nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=unidim_simplify,
                        snodes=snodes, normalize=no_stochastic_nodes )
        }
        #--- non-normal distribution
        if ( skillspace !="normal" ){        # non-normal distribution
            res <- tam_mml_3pl_stud_prior_discrete( pi.k=pi.k, ntheta=ntheta, G=G,
                        group1.list=group1.list, gwt=gwt )
            gwt <- res$gwt
        }

# cat("stud prior") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #******************************************
        #--- calculate student's likelihood
        gwt1 <- gwt
        res.hwt <- tam_calc_posterior( rprobs=rprobs, gwt=gwt1, resp=resp, nitems=nitems,
                        resp.ind.list=resp.ind.list, normalization=FALSE,
                        thetasamp.density=thetasamp.density, snodes=snodes, resp.ind=resp.ind,
                        logprobs=TRUE, avoid.zerosum=TRUE )
        hwt0 <- hwt <- res.hwt$hwt
        hwt <- tam_normalize_matrix_rows(hwt)
# cat("\nposterior v2") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        # collect old values for convergence indication
        oldxsi <- xsi
        oldbeta <- beta
        oldvariance <- variance

        #******************************************
        # M step: distribution parameter estimation of beta and variance
        if ( skillspace=="normal" ){
            resr <- tam_mml_3pl_mstep_regression( resp=resp, hwt=hwt, resp.ind=resp.ind,
                        pweights=pweights, pweightsM=pweightsM, Y=Y, theta=theta, theta2=theta2,
                        YYinv=YYinv, ndim=ndim, nstud=nstud, beta.fixed=beta.fixed,
                        variance=variance, Variance.fixed=variance.fixed, group=group, G=G,
                        snodes=snodes, thetasamp.density=thetasamp.density, nomiss=nomiss,
                        iter=iter, group_indices=group_indices,
                        variance_acceleration=variance_acceleration, beta=beta )
#     cat("m step regression") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
            beta <- resr$beta
            variance <- resr$variance
            itemwt <- resr$itemwt
            variance_acceleration <- resr$variance_acceleration
            beta_change <- resr$beta_change
            variance_change <- resr$variance_change
            if ( beta_change < conv) betaConv <- TRUE
            if ( variance_change < conv) varConv <- TRUE

        }  # end skillspace=="normal"
        #******************************************

        #******************************************
        # skill space estimation non-normal distribution
        # log-linear smoothing of skill space
        if ( skillspace !="normal" ){
            res <- tam_mml_3pl_skillspace( Ngroup=Ngroup, pi.k=pi.k,
                        delta.designmatrix=delta.designmatrix, G=G, delta=delta,
                        delta.fixed=delta.fixed, hwt=hwt, resp.ind=resp.ind,
                        pweightsM=pweightsM,
                        pweights=pweights, group1.list=group1.list,
                        delta_acceleration=delta_acceleration, iter=iter )
            pi.k <- res$pi.k
            delta <- res$delta
            covdelta <- res$covdelta
            itemwt <- res$itemwt
            delta_acceleration <- res$delta_acceleration
            delta_change <- res$delta_change
            varConv <- res$varConv
            betaConv <- res$betaConv
        }

        #******
        # generate input for fixed parameters
        xsi.fixed.estimated <- tam_generate_xsi_fixed_estimated( xsi=xsi, A=A )
        B.fixed.estimated <- tam_generate_B_fixed_estimated(B=B)

        ######################################
        # calculation of expected counts
        res <- tam_mml_3pl_expected_counts( datindw=datindw, nitems=nitems,
                    maxK=maxK, ntheta=ntheta, hwt=hwt)
        n.ik <- res$n.ik
        N.ik <- res$N.ik

# cat("\nexpected counts") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        ######################################
        # M-step item intercepts
        res <- tam_mml_3pl_mstep_item_intercepts( max.increment=max.increment, np=np,
                    est.xsi.index0=est.xsi.index0, Msteps=Msteps, nitems=nitems, A=A, AXsi=AXsi,
                    B=B, xsi=xsi, guess=guess, theta=theta, nnodes=nnodes, maxK=maxK,
                    progress=progress, itemwt=itemwt, indexIP.no=indexIP.no,
                    indexIP.list2=indexIP.list2, ItemScore=ItemScore, fac.oldxsi=fac.oldxsi,
                    rprobs=rprobs, xsi.fixed=xsi.fixed, convM=convM, rprobs0=rprobs0, n.ik=n.ik,
                    N.ik=N.ik, xsi.prior=xsi.prior, indexIP.list=indexIP.list,
                    xsi_acceleration=xsi_acceleration, iter=iter )
        xsi <- res$xsi
        se.xsi <- res$se.xsi
        xsi_change <- res$xsi_change
        xsi_acceleration <- res$xsi_acceleration

# cat("\nM steps intercepts") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1


        ###############################################
        # M-step item slopes
        if ( est.some.slopes){
            oldgamma <- gammaslope
            res <- tam_mml_3pl_mstep_item_slopes( max.increment=max.increment,
                        np=np, Msteps=Msteps, nitems=nitems, A=A, AXsi=AXsi, B=B,
                        xsi=xsi, guess=guess, theta=theta, nnodes=nnodes,
                        maxK=maxK, progress=progress, ItemScore=ItemScore,
                        fac.oldxsi=fac.oldxsi, rprobs=rprobs, xsi.fixed=xsi.fixed,
                        convM=convM, rprobs0=rprobs0, n.ik=n.ik, N.ik=N.ik,
                        gammaslope=gammaslope, E=E, FdesM=FdesM, dimFdes=dimFdes,
                        gammaslope.fixed=gammaslope.fixed,
                        gammaslope.prior=gammaslope.prior, maxgamma=maxgamma,
                        Edes=Edes, gammaslope.constr.V=gammaslope.constr.V, V1=V1,
                        e2=e2, gammaslope.center.index=gammaslope.center.index,
                        gammaslope.center.value=gammaslope.center.value,
                        userfct.gammaslope=userfct.gammaslope,
                        gammaslope_acceleration=gammaslope_acceleration, V=V,
                        skip_B=FALSE)
            gammaslope <- res$gammaslope
            se.gammaslope <- res$se.gammaslope
            gammaslope.change <- res$gammachange
            gammaslope_change <- res$gammaslope_change
            gammaslope_acceleration <- res$gammaslope_acceleration
            B <- res$B
        }



# cat("\nM steps slopes") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #--- guessing parameter estimation
        if ( est.some.guess ){
            oldguess <- guess
            res <-  tam_mml_3pl_mstep_item_guessing( guess=guess, Msteps=Msteps, convM=convM,
                        nitems=nitems, A=A, AXsi=AXsi, B=B, xsi=xsi, theta=theta, nnodes=nnodes,
                        maxK=maxK, n.ik=n.ik, N.ik=N.ik, est.guess=est.guess,
                        old.increment.guess=old.increment.guess, guess.prior=guess.prior,
                        progress=progress, max.guess=max.guess, guess_acceleration=guess_acceleration,
                        iter=iter )
            guess <- res$guess
            guess.change <- res$guess.change
            se.guess <- res$se.guess
            guess_acceleration <- res$guess_acceleration
        }
# cat("\nM steps guess") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #--- decrease increments in every iteration
        if( increment.factor > 1) max.increment <-  1 / increment.factor^iter

        #--- compute deviance
        res <- tam_mml_3pl_deviance( hwt0=hwt0, rfx=rfx, res.hwt=res.hwt, pweights=pweights,
                    snodes=snodes, deviance=deviance, deviance.history=deviance.history, iter=iter)
        deviance <- res$deviance
        rfx <- res$rfx
        deviance.history <- res$deviance.history
        a01 <- rel_deviance_change <- res$rel_deviance_change
        a02 <- deviance_change <- res$deviance_change

        if (con$dev_crit=="relative" ){ a02 <- a01 }

        if( ( deviance < deviance.min ) | ( iter==1)  ){
            xsi.min.deviance <- xsi
            beta.min.deviance <- beta
            variance.min.deviance <- variance
            hwt.min <- hwt
            AXsi.min <- AXsi
            B.min <- B
            gammaslope.min <- gammaslope
            se.gammaslope.min <- se.gammaslope
            deviance.min <- deviance
            delta.min <- delta
            itemwt.min <- itemwt
            se.xsi.min <- se.xsi
            se.B.min <- se.B
        }

        a1 <- xsi_change
        a2 <- beta_change
        a3 <- variance_change
        a44 <- max( a4, gammaslope.change, guess.change )
        a31 <- delta_change
        devch <- - ( deviance - olddeviance )

        res <-  tam_mml_progress_em( progress=progress, deviance=deviance,
                    deviance_change=deviance_change,
                    iter=iter, rel_deviance_change=rel_deviance_change, xsi_change=xsi_change,
                    beta_change=beta_change, variance_change=variance_change,
                    B_change=gammaslope.change, is_latreg=FALSE, is_mml_3pl=TRUE,
                    guess_change=guess.change, skillspace=skillspace, delta_change=delta_change,
                    devch=devch)

    } # end of EM algorithm loop
    ############################################################################

    xsi.min.deviance -> xsi
    beta.min.deviance -> beta
    variance.min.deviance -> variance
    hwt.min -> hwt
    AXsi.min -> AXsi
    B.min -> B
    gammaslope.min -> gammaslope
    se.gammaslope.min -> se.gammaslope
    delta.min -> delta
    deviance.min -> deviance
    itemwt.min -> itemwt
    se.xsi.min -> se.xsi
    se.B.min -> se.B

    #*** include NAs in AXsi
    AXsi <- tam_mml_include_NA_AXsi(AXsi=AXsi, maxcat=maxK, A=A, xsi=xsi)

    #**** standard errors AXsi
    se.AXsi <- tam_mml_se_AXsi( AXsi=AXsi, A=A, se.xsi=se.xsi, maxK=maxK )

    ##*** Information criteria
    ic <- tam_mml_3pl_ic( nstud=nstud100, deviance=deviance, xsi=xsi, xsi.fixed=xsi.fixed,
                beta=beta, beta.fixed=beta.fixed, ndim=ndim, variance.fixed=variance.fixed,
                G=G, irtmodel=irtmodel, B_orig=B_orig, B.fixed=B.fixed, E=E,
                est.variance=est.variance, resp=resp, est.slopegroups=est.slopegroups,
                skillspace=skillspace, delta=delta, delta.fixed=delta.fixed,
                est.guess=est.guess, fulldesign=fulldesign, est.some.slopes=est.some.slopes,
                gammaslope=gammaslope, gammaslope.fixed=gammaslope.fixed,
                gammaslope.constr.V=gammaslope.constr.V, gammaslope.constr.Npars=gammaslope.constr.Npars,
                gammaslope.center.index=gammaslope.center.index, gammaslope.prior=gammaslope.prior,
                numdiff.parm=5*1E-4, pweights=pweights, resp.ind=resp.ind)

    #***
    # calculate counts
    res <- tam_calc_counts( resp=resp, theta=theta, resp.ind=resp.ind, group=group,
                maxK=maxK, pweights=pweights, hwt=hwt )
    if ( skillspace=="normal"){
        n.ik <- res$n.ik
        pi.k <- res$pi.k
    }

    #****
    # collect item parameters
    item1 <- tam_mml_3pl_itempartable( resp=resp, maxK=maxK, AXsi=AXsi,
                    B=B, ndim=ndim, resp.ind=resp.ind, rprobs=rprobs,
                    n.ik=n.ik, pi.k=pi.k, guess=guess, est.guess=est.guess )
    #*** IRT parameterization
    item_irt <- tam_irt_parameterization(resp=resp, maxK=maxK, B=B, AXsi=AXsi,
                    irtmodel=irtmodel, tam_function="tam.mml.3pl",
                    skillspace=skillspace)

    # distribution moments
    if ( skillspace !="normal" ){
        D <- ncol(theta.k)
        moments <- tam_mml_3pl_distributionmoments( D=D, G=G, pi.k=pi.k, theta.k=theta.k )
    } else {
        moments <- NULL
    }

    #**** collect all person statistics
    res <- tam_mml_person_posterior( pid=pid, nstud=nstud, pweights=pweights,
                resp=resp, resp.ind=resp.ind, snodes=snodes,
                hwtE=hwt, hwt=hwt, ndim=ndim, theta=theta )
    person <- res$person
    EAP.rel <- res$EAP.rel

    ############################################################
    s2 <- Sys.time()
    if ( is.null( dimnames(A)[[3]] ) ){
      dimnames(A)[[3]] <- paste0("Xsi", 1:dim(A)[3] )
    }
    item <- data.frame( "xsi.index"=1:np,
                        "xsi.label"=dimnames(A)[[3]],
                        "est"=xsi )
    if (progress){
      cat(disp)
      cat("Item Parameters\n")
      item2 <- item
      item2[,"est"] <- round( item2[,"est"], 4 )
      print(item2)
      #**** skillspace==normal
      if (skillspace=="normal"){
          cat("...................................\n")
          cat("Regression Coefficients\n")
          print( beta, 4  )
          cat("\nVariance:\n" ) #, round( varianceM, 4 ))
          if ( G==1 ){
            varianceM <- matrix( variance, nrow=ndim, ncol=ndim )
            print( varianceM, 4 )
          } else {
            print( unlist(variance), 4 )
        }
          if ( ( ndim > 1 ) & ( G==1 ) ){
            cat("\nCorrelation Matrix:\n" ) #, round( varianceM, 4 ))
            print( cov2cor(varianceM), 4 )
                }
          }
      #****
      cat("\n\nEAP Reliability:\n")
      print( round(EAP.rel,3) )
      cat("\n-----------------------------")
      devmin <- which.min( deviance.history[,2] )
      if ( devmin < iter ){
        cat(paste("\n\nMinimal deviance at iteration ", devmin,
                  " with deviance ", round(deviance.history[ devmin, 2 ],3), sep=""), "\n")
        cat("The corresponding estimates are\n")
        cat("  xsi.min.deviance\n  beta.min.deviance \n  variance.min.deviance\n\n")
      }
      cat( "\nStart: ", paste(s1))
      cat( "\nEnd: ", paste(s2),"\n")
      print(s2-s1)
      cat( "\n" )
    }

    # collect xsi parameters
    obji <- data.frame( "xsi"=xsi, "se.xsi"=se.xsi )
    rownames(obji) <- dimnames(A)[[3]]
    xsi <- obji

    # labels gammaslope parameters
    names(gammaslope) <- dimnames(E)[[4]]

    #**** calculate individual likelihood
    res.hwt <- tam_calc_posterior( rprobs=rprobs, gwt=1+0*gwt, resp=resp, nitems=nitems,
                    resp.ind.list=resp.ind.list, normalization=FALSE,
                    thetasamp.density=thetasamp.density, snodes=snodes, resp.ind=resp.ind )
    res.like <- res.hwt[["hwt"]]

    # Output list
    deviance.history <- deviance.history[ 1:iter, ]
    res <- list( "xsi"=xsi,
                 "beta"=beta, "variance"=variance,
                 "moments"=moments,
                 "item"=item1, item_irt=item_irt,
                 "person"=person, pid=pid, "EAP.rel"=EAP.rel,
                 "post"=hwt,  "rprobs"=rprobs, "itemweight"=itemwt,
                 "theta"=theta,
                 "n.ik"=n.ik, "pi.k"=pi.k,
                 "Y"=Y, "resp"=resp0,
                 "resp.ind"=resp.ind, "group"=group,
                 "G"=if ( is.null(group)){1} else { length(unique( group ) )},
                 "groups"=if ( is.null(group)){1} else { groups },
                 "formulaY"=formulaY, "dataY"=dataY,
                 "pweights"=pweights0,
                 "time"=c(s1,s2), "A"=A, "B"=B,
                 "se.B"=se.B,
                 "nitems"=nitems, "maxK"=maxK, "AXsi"=AXsi,
                 "AXsi_"=- AXsi,
                 "se.AXsi"=se.AXsi,
                 "nstud"=nstud, "resp.ind.list"=resp.ind.list,
                 "hwt"=hwt, "like"=res.like, "ndim"=ndim,
                 "xsi.fixed"=xsi.fixed,
                 "xsi.fixed.estimated"=xsi.fixed.estimated,
                 "beta.fixed"=beta.fixed, "Q"=Q,
                 "B.fixed"=B.fixed,
                 "B.fixed.estimated"=B.fixed.estimated,
                 "est.slopegroups"=est.slopegroups, "E"=E, "basispar"=basispar,
                 "variance.fixed"=variance.fixed,
                 "nnodes"=nnodes, "deviance"=deviance,
                 "ic"=ic, thetasamp.density=thetasamp.density,
                 "deviance.history"=deviance.history,
                 "control"=con1a, "irtmodel"=irtmodel,
                 "iter"=iter,
                 "printxsi"=printxsi     , "YSD"=YSD        ,
                 "skillspace"=skillspace,
                 "delta"=delta, "delta.designmatrix"=delta.designmatrix,
                 "gammaslope"=gammaslope, "se.gammaslope"=se.gammaslope,
                 "guess"=guess,  "se.guess"=se.guess,
                 "E"=E, "Edes"=Edes, CALL=CALL
    )
    class(res) <- "tam.mml.3pl"
    return(res)
}

