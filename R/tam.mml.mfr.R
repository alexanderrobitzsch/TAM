## File Name: tam.mml.mfr.R
## File Version: 9.902
tam.mml.mfr <-
  function( resp, Y=NULL, group=NULL,  irtmodel="1PL",
            formulaY=NULL, dataY=NULL,
            ndim=1, pid=NULL,
            xsi.fixed=NULL,  xsi.setnull=NULL,
            xsi.inits=NULL,
            beta.fixed=NULL, beta.inits=NULL,
            variance.fixed=NULL, variance.inits=NULL,
            est.variance=TRUE, formulaA=~item+item:step, constraint="cases",
            A=NULL, B=NULL, B.fixed=NULL,
            Q=NULL, facets=NULL, est.slopegroups=NULL, E=NULL,
            pweights=NULL, verbose=TRUE, control=list(),
            delete.red.items=TRUE
            # control can be specified by the user
  ){

    CALL <- match.call()
    a0 <- Sys.time()
    s1 <- Sys.time()

    prior_list_xsi=NULL
    mstep_intercept_method <- "R"

    # display
    disp <- "....................................................\n"
    increment.factor <- progress <- nodes <- snodes <- ridge <- xsi.start0 <- QMC <- NULL
    maxiter <- conv <- convD <- min.variance <- max.increment <- Msteps <- convM <- NULL
    resp_orig <- resp
    B00 <- B
    B <- trim_increment <- NULL
    fac.oldxsi <- acceleration <- NULL

    #**** handle verbose argument
    args_CALL <- as.list( sys.call() )
    if ( ! tam_in_names_list( list=control, variable="progress" )     ){
        control$progress <- verbose
    }
    #*******
      #--- attach control elements
    e1 <- environment()
    tam_fct <- "tam.mml.mfr"
    res <- tam_mml_control_list_define(control=control, envir=e1, tam_fct=tam_fct,
                prior_list_xsi=prior_list_xsi)
    con <- res$con
    con1a <- res$con1a

    # userfct.variance is not allowed in tam.mml.mfr
    userfct.variance <- NULL

    #***
    fac.oldxsi <- max( 0, min( c( fac.oldxsi, .95 ) ) )

    if ( constraint=="items" ){ beta.fixed <- FALSE }

    pid0 <- pid <- unname(c(unlist(pid)))
    if (progress){
      cat(disp)
      cat("Processing Data     ", paste(Sys.time()), "\n") ; utils::flush.console()
    }
    if ( ! is.null(group) ){
      con1a$QMC <- QMC <- FALSE
      con1a$snodes <- snodes <- 0
    }

    resp <- as.matrix(resp)
    resp <- add.colnames.resp(resp)
    itemnames <- colnames(resp)

    nullY <- is.null(Y)

    if ( ! is.null(facets) ){
        facets <- as.data.frame(facets)
    }
# cat("read data" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1

    #--- compute maxKi
    res <- tam_mml_mfr_proc_compute_maxKi(resp=resp, facets=facets)
    maxKi <- res$maxKi

    #--- handle formula and facets
    resp00 <- resp
    res <- tam_mml_mfr_dataprep( formulaA=formulaA, xsi.setnull=xsi.setnull, B=B,
                Q=Q, resp=resp, pid=pid, facets=facets, beta.fixed=beta.fixed )
    formulaA <- res$formula_update
    xsi.setnull <- res$xsi.setnull
    beta.fixed <- res$beta.fixed
    facets <- res$facets
    PSF <- res$PSF
    pid <- res$pid
# cat(" mml mfr dataprep  " ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1

    #--- create design matrices
    res <-  tam_mml_mfr_proc_create_design_matrices( pid=pid, maxKi=maxKi, resp=resp,
                formulaA=formulaA, facets=facets, constraint=constraint, ndim=ndim, Q=Q,
                A=A, B=B, progress=progress, xsi.fixed=xsi.fixed, resp00=resp00, B00=B00,
                beta.fixed=beta.fixed )
    pid <- res$pid
    diffKi <- res$diffKi
    var_ki <- res$var_ki
    xsi.fixed <- res$xsi.fixed
    xsi.elim <- res$xsi.elim
    beta.fixed <- res$beta.fixed
    A <- res$A
    cA <- res$cA
    B <- res$B
    Q <- res$Q
    X <- res$X
    X.red <- res$X.red
    gresp <- res$gresp
    gresp.noStep <- res$gresp.noStep
    xsi.constr <- res$xsi.constr
    design <- res$design
# cat(" --- design matrix ready" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1

    #--- processing in case of multiple person IDs in a dataset
    tp <- max( table( pid ))
    if ( tp > 1){
        res <- tam_mml_mfr_proc_multiple_person_ids( pid=pid, tp=tp, gresp=gresp, gresp.noStep=gresp.noStep,
                    progress=progress )
        pid <- res$pid
        gresp <- res$gresp
        gresp.noStep <- res$gresp.noStep
    }
# cat("process data in case of multiple persons" ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1

    #--- set some xsi effects to zero
    res <- tam_mml_mfr_proc_xsi_setnull( xsi.setnull=xsi.setnull, A=A, xsi.fixed=xsi.fixed )
    xsi.fixed <- res$xsi.fixed
    xsi0 <- res$xsi0

    nitems <- nrow( X.red )
    nstud <- nrow(gresp)        # number of students
    if ( is.null( pweights) ){
        pweights <- rep(1,nstud) # weights of response pattern
    }

    if (progress){
        cat("    * Response Data:", nstud, "Persons and ",
            ncol(gresp.noStep), "Generalized Items (", paste(Sys.time()),")\n" )  ;
        utils::flush.console()
    }
    if ( is.null(pid) ){
        pid <- seq(1,nstud)
    }

    # normalize person weights to sum up to nstud
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
    res <- tam_mml_progress_proc_nodes( progress=progress, snodes=snodes, nnodes=nnodes,
                    skillspace="normal", QMC=QMC)

    #--- maximum no. of categories per item. Assuming dichotomous
    maxK <- max( resp, na.rm=TRUE ) + 1

    #--- number of parameters
    np <- dim(A)[[3]]

    #--- xsi parameter index
    res <- tam_mml_proc_est_xsi_index(A, xsi.inits, xsi.fixed)
    np <- res$np
    xsi <- res$xsi
    est.xsi.index0 <- est.xsi.index <- res$est.xsi.index

    #--- inits variance
    res <- tam_mml_inits_variance( variance.inits=variance.inits, ndim=ndim, variance.fixed=variance.fixed )
    variance <- res$variance

    #--- inits group
    res <- tam_mml_inits_groups( group=group )
    G <- res$G
    groups <- res$groups
    group <- res$group
    var.indices <- res$var.indices

    #--- inits beta
    res <- tam_mml_mfr_inits_beta( Y=Y, formulaY=formulaY, dataY=dataY, G=G, group=group, groups=groups,
                nstud=nstud, pweights=pweights, ridge=ridge, beta.fixed=beta.fixed, xsi.fixed=xsi.fixed,
                constraint=constraint, ndim=ndim, beta.inits=beta.inits, tp=tp, gresp=gresp,
                pid0=pid0 )
    Y <- res$Y
    nullY <- res$nullY
    formulaY <- res$formulaY
    nreg <- res$nreg
    W <- res$W
    YYinv <- res$YYinv
    beta.fixed <- res$beta.fixed
    beta <- res$beta

    #--- response indicators
    res <- tam_mml_mfr_proc_response_indicators(nitems, gresp, gresp.noStep)
    resp.ind.list <- res$resp.ind.list
    gresp.ind <- res$gresp.ind
    gresp.noStep.ind <- res$gresp.noStep.ind
    resp.ind <- res$resp.ind
    nomiss <- res$nomiss
    miss.items <- res$miss.items
    gresp0.noStep <- res$gresp0.noStep
    gresp <- res$gresp
    gresp.noStep <- res$gresp.noStep

    #-- delete items with only missing responses
    res <- tam_mml_mfr_proc_delete_missing_items( miss.items=miss.items,
                delete.red.items=delete.red.items, maxK=maxK,
                gresp=gresp, gresp.noStep=gresp.noStep, gresp.noStep.ind=gresp.noStep.ind,
                A=A, B=B, resp.ind.list=resp.ind.list, resp.ind=resp.ind, nitems=nitems,
                pweightsM=pweightsM, pweights=pweights, nstud=nstud, progress=progress )
    miss.itemsK  <- res$miss.itemsK
    miss.items <- res$miss.items
    delete.red.items <- res$delete.red.items
    A <- res$A
    B <- res$B
    gresp <- res$gresp
    gresp.noStep <- res$gresp.noStep
    gresp.noStep.ind <- res$gresp.noStep.ind
    resp.ind.list <- res$resp.ind.list
    resp.ind <- res$resp.ind
    nitems <- res$nitems
    pweightsM <- res$pweightsM

    #-- AXsi
    AXsi <- matrix(0,nrow=nitems,ncol=maxK )  #A times xsi

    #--- parameter indices xsi parameters
    res <- tam_mml_proc_xsi_parameter_index_A(A=A, np=np)
    indexIP <- res$indexIP
    indexIP.list <- res$indexIP.list
    indexIP.list2 <- res$indexIP.list2
    indexIP.no <- res$indexIP.no

    #--- sufficient statistics for item parameters
    cA <- t( matrix( aperm( A, c(2,1,3) ), nrow=dim(A)[3], byrow=TRUE ) )
    res <- tam_mml_sufficient_statistics( nitems=nitems, maxK=maxK, resp=gresp.noStep,
                    resp.ind=gresp.noStep.ind,     pweights=pweights, cA=cA, progress=progress )
    ItemScore <- res$ItemScore
    cResp <- res$cResp
    col.index <- res$col.index

    #--- inits xsi
    res <- tam_mml_mfr_inits_xsi( gresp.noStep.ind=gresp.noStep.ind, col.index=col.index, cA=cA,
                pweights=pweights, xsi=xsi, xsi.start0=xsi.start0, resp=resp, A=A,
                xsi.inits=xsi.inits, xsi.fixed=xsi.fixed, ItemScore=ItemScore, est.xsi.index=est.xsi.index )
    xsi <- res$xsi
    ItemMax <- res$ItemMax

    #--- prior distribution xsi
    prior_list_xsi <- tam_mml_proc_prior_list_xsi( prior_list_xsi=prior_list_xsi, xsi=xsi )

    xsi.min.deviance <- xsi
    beta.min.deviance <- beta
    variance.min.deviance <- variance

    #--- create grid of nodes for numeric or stochastic integration
    res <- tam_mml_create_nodes( snodes=snodes, nodes=nodes, ndim=ndim, QMC=QMC )
    theta <- res$theta
    theta2 <- res$theta2
    thetawidth <- res$thetawidth
    theta0.samp <- res$theta0.samp
    thetasamp.density <- res$thetasamp.density

    deviance <- 0
    deviance.history <- tam_deviance_history_init(maxiter=maxiter)

    iter <- 0
    a02 <- a1 <- 999    # item parameter change
    a4 <- 0

    hwt.min <- 0
    rprobs.min <- 0
    AXsi.min <- 0
    B.min <- 0
    deviance.min <- 1E100
    itemwt.min <- 0

    #--- create unidim_simplify
    res <- tam_mml_proc_unidim_simplify( Y=Y, A=A, G=G, beta.fixed=beta.fixed )
    unidim_simplify <- res$unidim_simplify
    YSD <- res$YSD
    Avector <- res$Avector

    #--- acceleration
    res <- tam_acceleration_inits(acceleration=acceleration, G=G, xsi=xsi,
                variance=variance)
    xsi_acceleration <- res$xsi_acceleration
    variance_acceleration <- res$variance_acceleration

    #--- warning multiple group estimation
    res <- tam_mml_warning_message_multiple_group_models( ndim=ndim, G=G)

    #--- compute some arguments for EM algorithm
    maxcat <- tam_rcpp_mml_maxcat(A=as.vector(A), dimA=dim(A) )

    ##**SE
    se.xsi <- 0*xsi
    se.B <- 0*B
    se.xsi.min <- se.xsi
    se.B.min <- se.B
    devch <- 0

    # display
    disp <- "....................................................\n"
    # define progress bar for M step
# cat("rest  " ) ; a1 <- Sys.time() ; print(a1-a0) ; a0 <- a1

    ##############################################################
    ##############################################################
    ##############################################################
    #Start EM loop here
    while ( ( (!betaConv | !varConv)  | ((a1 > conv) | (a4 > conv) | (a02 > convD)) )  &
              (iter < maxiter) ) {

        # a0 <- Sys.time()
        iter <- iter + 1
        #--- progress
        res <- tam_mml_progress_em0(progress=progress, iter=iter, disp=disp)
        # calculate nodes for Monte Carlo integration
        if ( snodes > 0){
            res <- tam_mml_update_stochastic_nodes( theta0.samp=theta0.samp, variance=variance,
                        snodes=snodes, beta=beta, theta=theta )
            theta <- res$theta
            theta2 <- res$theta2
            thetasamp.density <- res$thetasamp.density
        }
        olddeviance <- deviance
        #--- calculation of probabilities
        res <- tam_mml_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi, theta=theta, nnodes=nnodes,
                    maxK=maxK, recalc=TRUE, maxcat=maxcat, use_rcpp=TRUE )
      # cat("calc prob") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
        rprobs <- res$rprobs
        AXsi <- res$AXsi

        #--- calculate student's prior distribution
        gwt <- tam_stud_prior( theta=theta, Y=Y, beta=beta, variance=variance, nstud=nstud,
                    nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=unidim_simplify,
                    snodes=snodes )

        #--- calculate student's likelihood
        res.hwt <- tam_calc_posterior( rprobs=rprobs, gwt=gwt, resp=gresp.noStep, nitems=nitems,
                        resp.ind.list=resp.ind.list, normalization=TRUE,
                        thetasamp.density=thetasamp.density, snodes=snodes, resp.ind=resp.ind,
                        avoid.zerosum=TRUE )
        hwt <- res.hwt$hwt

        #--- M step: estimation of beta and variance
        resr <- tam_mml_mstep_regression( resp=gresp.noStep, hwt=hwt,
                    resp.ind=gresp.noStep.ind, pweights=pweights, pweightsM=pweightsM,
                    Y=Y, theta=theta, theta2=theta2, YYinv=YYinv, ndim=ndim, nstud=nstud,
                    beta.fixed=beta.fixed, variance=variance, Variance.fixed=variance.fixed,
                    group=group, G=G, snodes=snodes, nomiss=nomiss, iter=iter,
                    min.variance=min.variance, userfct.variance=userfct.variance,
                    variance_acceleration=variance_acceleration, est.variance=est.variance,
                    beta=beta )
        beta <- resr$beta
        variance <- resr$variance
        itemwt <- resr$itemwt
        variance_acceleration <- resr$variance_acceleration
        variance_change <- resr$variance_change
        beta_change <- resr$beta_change

        if ( beta_change < conv){ betaConv <- TRUE }
        if ( variance_change < conv){ varConv <- TRUE }

        #--- M-step item intercepts
        if (mstep_intercept_method=="optim"){
            res <- tam_calc_counts( resp=gresp.noStep, theta=theta, resp.ind=gresp.noStep.ind, group=group,
                        maxK=maxK, pweights=pweights, hwt=hwt )
            n.ik <- res$n.ik
        }
        res <- tam_mml_mstep_intercept( A=A, xsi=xsi, AXsi=AXsi, B=B, theta=theta,
                    nnodes=nnodes, maxK=maxK, Msteps=Msteps, rprobs=rprobs, np=np,
                    est.xsi.index0=est.xsi.index0, itemwt=itemwt, indexIP.no=indexIP.no,
                    indexIP.list2=indexIP.list2, Avector=Avector, max.increment=max.increment,
                    xsi.fixed=xsi.fixed, fac.oldxsi=fac.oldxsi, ItemScore=ItemScore,
                    convM=convM, progress=progress, nitems=nitems, iter=iter,
                    increment.factor=increment.factor, xsi_acceleration=xsi_acceleration,
                    trim_increment=trim_increment, prior_list_xsi=prior_list_xsi,
                    mstep_intercept_method=mstep_intercept_method, n.ik=n.ik, maxcat=maxcat )
        xsi <- res$xsi
        se.xsi <- res$se.xsi
        max.increment <- res$max.increment
        xsi_acceleration <- res$xsi_acceleration
        xsi_change <- res$xsi_change
        logprior_xsi <- res$logprior_xsi

        #--- compute deviance
        res <- tam_mml_compute_deviance( loglike_num=res.hwt$rfx, loglike_sto=res.hwt$rfx,
                    snodes=snodes, thetawidth=thetawidth, pweights=pweights, deviance=deviance,
                    deviance.history=deviance.history, iter=iter, logprior_xsi=logprior_xsi )
        deviance <- res$deviance
        deviance.history <- res$deviance.history
        a01 <- rel_deviance_change <- res$rel_deviance_change
        a02 <- deviance_change <- res$deviance_change
        if (con$dev_crit=="relative" ){ a02 <- a01 }
        penalty_xsi <- res$penalty_xsi
        deviance_change_signed <- res$deviance_change_signed

        if( deviance < deviance.min ){
            xsi.min.deviance <- xsi
            beta.min.deviance <- beta
            variance.min.deviance <- variance
            hwt.min <- hwt
            AXsi.min <- AXsi
            B.min <- B
            deviance.min <- deviance
            itemwt.min <- itemwt
            se.xsi.min <- se.xsi
            se.B.min <- se.B
        }

        a1 <- xsi_change
        a2 <- beta_change
        a3 <- variance_change
        devch <- - ( deviance - olddeviance )

        #--- print progress
        res <- tam_mml_progress_em( progress=progress, deviance=deviance, deviance_change=deviance_change,
                    iter=iter, rel_deviance_change=rel_deviance_change, xsi_change=xsi_change,
                    beta_change=beta_change, variance_change=variance_change, B_change=0,
                    devch=devch, penalty_xsi=penalty_xsi )

    } # end of EM loop
    #############################################################
    #############################################################
    xsi.min.deviance -> xsi
    beta.min.deviance -> beta
    variance.min.deviance -> variance
    hwt.min -> hwt
    AXsi.min -> AXsi
    B.min -> B
    deviance.min -> deviance
    itemwt.min -> itemwt
    se.xsi.min -> se.xsi
    se.B.min -> se.B
    #******
    #***
    resp <- gresp0.noStep
    resp.ind <- gresp.noStep.ind

    #****
    # look for non-estimable xsi parameters
#    xsi[ xsi==99 ] <- NA

    #******
    # generate input for fixed parameters
    xsi.fixed.estimated <- generate.xsi.fixed.estimated( xsi, A )
    B.fixed.estimated <- generate.B.fixed.estimated(B)

    #**** standard errors AXsi
    se.AXsi <- tam_mml_se_AXsi( AXsi=AXsi, A=A, se.xsi=se.xsi, maxK=maxK )

    ##*** information criteria
    ic <- tam_mml_ic( nstud=nstud, deviance=deviance, xsi=xsi, xsi.fixed=xsi.fixed,
                beta=beta, beta.fixed=beta.fixed, ndim=ndim,
                variance.fixed=variance.fixed, G=G, irtmodel=irtmodel, B_orig=NULL,
                B.fixed=B.fixed, E=E, est.variance=TRUE, resp=resp,
                est.slopegroups=NULL, variance.Npars=NULL, group=group, penalty_xsi=penalty_xsi )

    #***
    # calculate counts
    res <- tam_calc_counts( resp=gresp.noStep, theta=theta, resp.ind=gresp.noStep.ind,
                group=group, maxK=maxK, pweights=pweights, hwt=hwt )
    n.ik <- res$n.ik
    pi.k <- res$pi.k

    #--- collect item parameters
    item1 <- tam_itempartable( resp=gresp.noStep, maxK=maxK, AXsi=AXsi, B=B,
                    ndim=ndim, resp.ind=gresp.noStep.ind,
                    rprobs=rprobs, n.ik=n.ik, pi.k=pi.k, order=TRUE )

    #--- collect all person statistics
    res <- tam_mml_person_posterior( pid=pid, nstud=nstud, pweights=pweights,
                resp=gresp.noStep, resp.ind=gresp.noStep.ind, snodes=snodes,
                hwtE=hwt, hwt=hwt, ndim=ndim, theta=theta )
    person <- res$person
    EAP.rel <- res$EAP.rel

    #******
    s2 <- Sys.time()

    item <- data.frame( "xsi.index"=1:np,
                        "xsi.label"=dimnames(A)[[3]],
                        "est"=xsi )

    if (progress){
        cat(disp)
        cat("Item Parameters\n")
        item2 <- item
        item2[,"est"] <- round( item2[,"est"], 4 )
        print(item2)
        cat("...................................\n")
        cat("Regression Coefficients\n")
        print( beta, 4  )
        cat("\nVariance:\n" ) #, round( varianceM, 4 ))
        if (G==1 ){
            varianceM <- matrix( variance, nrow=ndim, ncol=ndim )
            print( varianceM, 4 )
        } else {
            print( variance[ var.indices], 4 )
        }
        if ( ndim > 1){
                cat("\nCorrelation Matrix:\n" ) #, round( varianceM, 4 ))
            print( stats::cov2cor(varianceM), 4 )
        }
        cat("\n\nEAP Reliability:\n")
        print( round (EAP.rel,3) )
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

    #--- collect xsi parameters
    res <- tam_mml_mfr_collect_xsi_parameters( xsi.constr=xsi.constr, resp=resp, A=A, xsi=xsi,
                se.xsi=se.xsi, delete.red.items=delete.red.items, itemnames=itemnames,
                miss.items=miss.items )
    resp <- res$resp
    xsi <- res$xsi
    xsi.facets <- res$xsi.facets

    #--- recompute posterior
    res.hwt <- tam_calc_posterior( rprobs=rprobs, gwt=1+0*gwt, resp=resp, nitems=nitems,
                    resp.ind.list=resp.ind.list, normalization=FALSE, thetasamp.density=thetasamp.density,
                    snodes=snodes, resp.ind=resp.ind )
    res.like <- res.hwt$hwt

    #***** standardized coefficients
    latreg_stand <- tam_latent_regression_standardized_solution(variance=variance, beta=beta, Y=Y)

    #--- OUTPUT LIST
    deviance.history <- deviance.history[ 1:iter, ]
    res <- list( "xsi"=xsi, "xsi.facets"=xsi.facets,
                 "beta"=beta, "variance"=variance,
                 "item"=item1,
                 "person"=person, pid=pid, "EAP.rel"=EAP.rel,
                 "post"=hwt,  "rprobs"=rprobs, "itemweight"=itemwt,
                 "theta"=theta,
                 "n.ik"=n.ik, "pi.k"=pi.k,
                 "Y"=Y, "resp"=resp,
                 "resp.ind"=resp.ind, "group"=group,
                 "G"=if ( is.null(group)){1} else { length(unique( group ) )},
                 "groups"=if ( is.null(group)){1} else { groups },
                 "formulaY"=formulaY, "dataY"=dataY,
                 "pweights"=pweights,
                 "time"=c(s1,s2,s2-s1), "A"=A, "B"=B,
                 "se.B"=se.B,
                 "nitems"=nitems, "maxK"=maxK, "AXsi"=AXsi,
                 "AXsi_"=- AXsi,
                 "se.AXsi"=se.AXsi,
                 "nstud"=nstud, "resp.ind.list"=resp.ind.list,
                 "hwt"=hwt, "like"=res.like, "ndim"=ndim,
                 "xsi.fixed"=xsi.fixed,
                 "xsi.fixed.estimated"=xsi.fixed.estimated,
                 "B.fixed.estimated"=B.fixed.estimated,
                 "beta.fixed"=beta.fixed, "Q"=Q,
                 "formulaA"=formulaA, "facets"=facets,
                 "xsi.constr"=xsi.constr,
                 "variance.fixed"=variance.fixed,
                 "nnodes"=nnodes, "deviance"=ic$deviance,
                 "ic"=ic, thetasamp.density=thetasamp.density,
                 "deviance.history"=deviance.history,
                 "control"=con1a, "irtmodel"=irtmodel,
                 "iter"=iter, "resp_orig"=resp_orig,
                 "printxsi"=TRUE, "YSD"=YSD, "PSF"=PSF,
                 CALL=CALL, latreg_stand=latreg_stand,
                  prior_list_xsi=prior_list_xsi, penalty_xsi=penalty_xsi
    )
    class(res) <- "tam.mml"
    return(res)
 }
