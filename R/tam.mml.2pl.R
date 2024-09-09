## File Name: tam.mml.2pl.R
## File Version: 9.593

tam.mml.2pl <- function( resp, Y=NULL, group=NULL,  irtmodel="2PL",
                 formulaY=NULL, dataY=NULL,
                 ndim=1, pid=NULL,
                 xsi.fixed=NULL,  xsi.inits=NULL,
                 beta.fixed=NULL, beta.inits=NULL,
                 variance.fixed=NULL, variance.inits=NULL,
                 est.variance=FALSE,
                 A=NULL, B=NULL, B.fixed=NULL,
                 Q=NULL, est.slopegroups=NULL, E=NULL,
                 gamma.init=NULL,
                 pweights=NULL,
                 userfct.variance=NULL, variance.Npars=NULL,
                 item.elim=TRUE,     verbose=TRUE,
                 control=list()
                 # control can be specified by the user
                 )
{

    s1 <- Sys.time()
    CALL <- match.call()
    # display
    disp <- "....................................................\n"
    increment.factor <- progress <- nodes <- snodes <- ridge <- xsi.start0 <- QMC <- NULL
    maxiter <- conv <- convD <- min.variance <- max.increment <- Msteps <- convM <- NULL
    R <- trim_increment <- NULL
    fac.oldxsi <- acceleration <- NULL

    #**** handle verbose argument
    args_CALL <- as.list( sys.call() )
    if ( ! tam_in_names_list( list=control, variable="progress" )     ){
        control$progress <- verbose
    }
    #*******

    if ( is.null(A)){ printxsi <- FALSE  } else { printxsi <- TRUE }

      #--- attach control elements
    e1 <- environment()
    tam_fct <- "tam.mml.2pl"

    res <- tam_mml_control_list_define(control=control, envir=e1, tam_fct=tam_fct)
    con <- res$con
    con1a <- res$con1a

    resp <- as.matrix(resp)
    resp0 <- resp <- add.colnames.resp(resp)

    if (progress){
        cat(disp)
        cat("Processing Data     ", paste(Sys.time()), "\n") ; utils::flush.console()
    }
    if ( ! is.null(group) ){
        con1a$QMC <- QMC <- FALSE
        con1a$snodes <- snodes <- 0
    }

  # define design matrix in case of PCM2
#  if (( irtmodel=="PCM2" ) & (is.null(Q)) & ( is.null(A)) ){
#            A <- .A.PCM2( resp )
#                    }
    if (( irtmodel=="PCM2" ) & ( is.null(A)) ){
      A <- .A.PCM2( resp )
    }

    if ( ! is.null( variance.fixed ) ){
        est.variance <- TRUE
    }


    nitems <- ncol(resp)       # number of items
    nstud <- nrow(resp)        # number of students

    #*****
    nstud100 <- sum(1*( rowSums( 1 - is.na(resp) ) > 0 ))

    if ( is.null( pweights) ){
        pweights <- rep(1,nstud) # weights of response pattern
    }

    if (progress){
        cat("    * Response Data:", nstud, "Persons and ",
                nitems, "Items \n" )  ;
        utils::flush.console()
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
    res <- tam_mml_progress_proc_nodes( progress=progress, snodes=snodes, nnodes=nnodes,
                    skillspace="normal", QMC=QMC)

    # maximum no. of categories per item. Assuming dichotomous
    maxK <- max( resp, na.rm=TRUE ) + 1

    #****
    # ARb 2015-12-08
    maxKi <- NULL
    if ( ! (item.elim ) ){
        maxKi <- rep( maxK - 1, ncol(resp) )
                }
    #***

  # create design matrices
  design <- designMatrices( modeltype="PCM", maxKi=maxKi, resp=resp,
                            A=A, B=B, Q=Q, R=R, ndim=ndim )
  A <- design$A
  B <- design$B
  cA <- design$flatA
  cA[is.na(cA)] <- 0
  if (progress){
      cat("    * Created Design Matrices   (",
            paste(Sys.time()), ")\n") ; utils::flush.console()
                }
  design <- NULL

  #---2PL---
  B_orig <- B  #keep a record of generated B before estimating it in 2PL model
  #---end 2PL---
  ################################
  # number of parameters
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

    #--- inits beta regression coefficients
    res <-  tam_mml_inits_beta( Y=Y, formulaY=formulaY, dataY=dataY, G=G, group=group,
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
  #---2PL---
        a4 <- 999
        basispar <- NULL
#        } else{  a4 <- 0 }

  if (irtmodel=="GPCM.design" ){
        ES <- ncol(E)
        basispar <- matrix( 1, ES, ndim )
        basispar1 <- solve( t(E) %*% E    , t(E) %*% rep(1,nitems))
        if ( ! is.null(gamma.init) ){
            basispar1 <- gamma.init
                        }

        for ( dd in 1:ndim){
            basispar[,dd] <- basispar1
                        }
               }


   if ( irtmodel %in% c("2PL","GPCM", "GPCM.design","2PL.groups", "GPCM.groups") ){
    if ( ! is.null(B.fixed) ){
            B[ B.fixed[,1:3,drop=FALSE] ] <- B.fixed[,4]
            B_orig[ B.fixed[,1:3,drop=FALSE] ] <- 0
                        }
                        }


  #---end 2PL---

    #--- create unidim_simplify
    res <- tam_mml_proc_unidim_simplify( Y=Y, A=A, G=G, beta.fixed=beta.fixed )
    unidim_simplify <- res$unidim_simplify
    YSD <- res$YSD
    Avector <- res$Avector

    #--- acceleration
    res <- tam_acceleration_inits(acceleration=acceleration, G=G, xsi=xsi,
                variance=variance, B=B, irtmodel=irtmodel )
    xsi_acceleration <- res$xsi_acceleration
    variance_acceleration <- res$variance_acceleration
    B_acceleration <- res$B_acceleration

    #--- warning multiple group estimation
    res <- tam_mml_warning_message_multiple_group_models( ndim=ndim, G=G)

    #--- compute some arguments for EM algorithm
    maxcat <- tam_rcpp_mml_maxcat(A=as.vector(A), dimA=dim(A) )

    ##**SE
    se.xsi <- 0*xsi
    se.B <- 0*B


          hwt.min <- 0
        rprobs.min <- 0
        AXsi.min <- 0
        B.min <- 0
        deviance.min <- 1E100
        itemwt.min <- 0
        se.xsi.min <- se.xsi
        se.B.min <- se.B
        B_change <- 0

  # display
  disp <- "....................................................\n"
  # define progress bar for M step
  mpr <- round( seq( 1, np, len=10 ) )

  ##############################################################
  #Start EM loop here
    while ( ( (!betaConv | !varConv)  | ((a1 > conv) | (a4 > conv) | (a02 > convD)) )  & (iter < maxiter) ) {

        a0 <- Sys.time()
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
        res <- tam_mml_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi, theta=theta,
                    nnodes=nnodes, maxK=maxK, recalc=TRUE, maxcat=maxcat, use_rcpp=TRUE )
        rprobs <- res$rprobs
        AXsi <- res$AXsi
# cat("calc_prob") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #--- calculate student's prior distribution
        gwt <- tam_stud_prior( theta=theta, Y=Y, beta=beta, variance=variance, nstud=nstud,
                    nnodes=nnodes, ndim=ndim, YSD=YSD, unidim_simplify=unidim_simplify,
                    snodes=snodes )
# cat("stud prior") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #--- calculate student's likelihood
        res.hwt <- tam_calc_posterior( rprobs=rprobs, gwt=gwt, resp=resp, nitems=nitems,
                        resp.ind.list=resp.ind.list, normalization=TRUE,
                        thetasamp.density=thetasamp.density, snodes=snodes,
                        resp.ind=resp.ind, avoid.zerosum=TRUE )
        hwt <- res.hwt$hwt
# cat("posterior v2") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        # M step: estimation of beta and variance
        resr <- tam_mml_mstep_regression( resp=resp, hwt=hwt, resp.ind=resp.ind,
                    pweights=pweights, pweightsM=pweightsM, Y=Y, theta=theta, theta2=theta2,
                    YYinv=YYinv, ndim=ndim, nstud=nstud, beta.fixed=beta.fixed, variance=variance,
                    Variance.fixed=variance.fixed, group=group, G=G, snodes=snodes,
                    thetasamp.density=thetasamp.density, nomiss=nomiss, iter=iter,
                    min.variance=min.variance, userfct.variance=userfct.variance,
                    variance_acceleration=variance_acceleration, est.variance=est.variance,
                    beta=beta )
# cat("m step regression") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        beta <- resr$beta
        variance <- resr$variance
        itemwt <- resr$itemwt
        variance_acceleration <- resr$variance_acceleration
        variance_change <- resr$variance_change
        beta_change <- resr$beta_change

        if ( beta_change < conv){ betaConv <- TRUE }
        if ( variance_change < conv){ varConv <- TRUE }

        #--- sufficient statistics item slopes
        res <- tam_mml_2pl_sufficient_statistics_item_slope( hwt=hwt, theta=theta,
                    cResp=cResp, pweights=pweights, maxK=maxK, nitems=nitems, ndim=ndim )
        thetabar <- res$thetabar
        cB_obs <- res$cB_obs
        B_obs <- res$B_obs
# cat("sufficient statistics 2PL") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        ######################################
        # M-step item intercepts
        res <- tam_mml_mstep_intercept( A=A, xsi=xsi, AXsi=AXsi, B=B, theta=theta,
                    nnodes=nnodes, maxK=maxK, Msteps=Msteps, rprobs=rprobs, np=np,
                    est.xsi.index0=est.xsi.index0, itemwt=itemwt, indexIP.no=indexIP.no,
                    indexIP.list2=indexIP.list2, Avector=Avector, max.increment=max.increment,
                    xsi.fixed=xsi.fixed, fac.oldxsi=fac.oldxsi, ItemScore=ItemScore,
                    convM=convM, progress=progress, nitems=nitems, iter=iter,
                    increment.factor=increment.factor, xsi_acceleration=xsi_acceleration,
                    trim_increment=trim_increment, maxcat=maxcat )
        xsi <- res$xsi
        se.xsi <- res$se.xsi
        max.increment <- res$max.increment
        xsi_acceleration <- res$xsi_acceleration
        xsi_change <- res$xsi_change
# cat("M steps intercepts") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

    #---2PL---
    if (irtmodel %in% c("2PL","GPCM","GPCM.design","2PL.groups", "GPCM.groups") ) {
        res <- tam_mml_2pl_mstep_slope( B_orig=B_orig, B=B, B_obs=B_obs, B.fixed=B.fixed,
                    max.increment=max.increment, nitems=nitems, A=A, AXsi=AXsi, xsi=xsi,
                    theta=theta, nnodes=nnodes, maxK=maxK, itemwt=itemwt, Msteps=Msteps,
                    ndim=ndim, convM=convM, irtmodel=irtmodel, progress=progress,
                    est.slopegroups=est.slopegroups, E=E, basispar=basispar,
                    se.B=se.B, equal.categ=equal.categ, B_acceleration=B_acceleration,
                    trim_increment=trim_increment, iter=iter, maxcat=maxcat, use_rcpp=TRUE,
                    use_rcpp_calc_prob=TRUE )
        B <- res$B
        se.B <- res$se.B
        basispar <- res$basispar
        B_acceleration <- res$B_acceleration
        a4 <- B_change <- res$B_change
    }
    #---end 2PL---

# cat("M steps slopes") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1

        #--- compute deviance
        res <- tam_mml_compute_deviance( loglike_num=res.hwt$rfx, loglike_sto=res.hwt$rfx,
                    snodes=snodes, thetawidth=thetawidth, pweights=pweights, deviance=deviance,
                    deviance.history=deviance.history, iter=iter )
        deviance <- res$deviance
        deviance.history <- res$deviance.history
        a01 <- rel_deviance_change <- res$rel_deviance_change
        a02 <- deviance_change <- res$deviance_change

    if (con$dev_crit=="relative" ){ a02 <- a01 }


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

        #** print progress
        res <- tam_mml_progress_em( progress=progress, deviance=deviance,
                    deviance_change=deviance_change, iter=iter,
                    rel_deviance_change=rel_deviance_change, xsi_change=xsi_change,
                    beta_change=beta_change, variance_change=variance_change,
                    B_change=B_change, devch=devch )

    } # end of EM loop
    #******************************************************

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

    #*** include NAs in AXsi
    AXsi <- tam_mml_include_NA_AXsi(AXsi=AXsi, maxcat=maxcat, A=A, xsi=xsi)

    #******
    # generate input for fixed parameters
    xsi.fixed.estimated <- tam_generate_xsi_fixed_estimated( xsi=xsi, A=A )
    B.fixed.estimated <- tam_generate_B_fixed_estimated(B=B)

    #**** standard errors AXsi
    se.AXsi <- tam_mml_se_AXsi( AXsi=AXsi, A=A, se.xsi=se.xsi, maxK=maxK )

    ##*** Information criteria
    ic <- tam_mml_ic( nstud=nstud100, deviance=deviance, xsi=xsi, xsi.fixed=xsi.fixed,
                beta=beta, beta.fixed=beta.fixed, ndim=ndim, variance.fixed=variance.fixed,
                G=G, irtmodel=irtmodel, B_orig=B_orig, B.fixed=B.fixed, E=E,
                est.variance=est.variance, resp=resp, est.slopegroups=est.slopegroups,
                variance.Npars=variance.Npars, group=group, AXsi=AXsi,
                pweights=pweights, resp.ind=resp.ind, B=B)

    #*** calculate counts
    res <- tam_calc_counts( resp=resp, theta=theta, resp.ind=resp.ind, group=group,
                maxK=maxK, pweights=pweights, hwt=hwt )
    n.ik <- res$n.ik
    pi.k <- res$pi.k

    #*** collect item parameters
    item1 <- tam_itempartable( resp=resp, maxK=maxK, AXsi=AXsi, B=B, ndim=ndim,
                resp.ind=resp.ind, rprobs=rprobs, n.ik=n.ik, pi.k=pi.k, pweights=pweights)

    #*** IRT parameterization
    item_irt <- tam_irt_parameterization(resp=resp, maxK=maxK, B=B, AXsi=AXsi,
                    irtmodel=irtmodel, tam_function="tam.mml.2pl")

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
    cat("...................................\n")
    cat("Regression Coefficients\n")
    print( beta, 4  )
    cat("\nVariance:\n" ) #, round( varianceM, 4 ))
    if (G==1 ){
      varianceM <- matrix( variance, nrow=ndim, ncol=ndim )
      print( varianceM, 4 )
    } else {
      print( variance[ var.indices], 4 )    }
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

    # collect xsi parameters
     obji <- data.frame( "xsi"=xsi, "se.xsi"=se.xsi )
    rownames(obji) <- dimnames(A)[[3]]
    xsi <- obji

     res.hwt <- tam_calc_posterior(rprobs=rprobs, gwt=1+0*gwt, resp=resp, nitems=nitems,
                                   resp.ind.list=resp.ind.list, normalization=FALSE,
                                   thetasamp.density=thetasamp.density, snodes=snodes,
                                   resp.ind=resp.ind )
      res.like <- res.hwt[["hwt"]]

    #***** standardized coefficients
    latreg_stand <- tam_latent_regression_standardized_solution(variance=variance, beta=beta, Y=Y)

  # Output list
  deviance.history <- deviance.history[ 1:iter, ]
  res <- list( "xsi"=xsi,
               "beta"=beta, "variance"=variance,
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
               "hwt"=hwt, "like"=res.like,
               "ndim"=ndim,
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
                "printxsi"=printxsi, "YSD"=YSD, CALL=CALL,
                latreg_stand=latreg_stand )
  class(res) <- "tam.mml"
  return(res)
}


# tam.mml.output <- function(){
#     }
