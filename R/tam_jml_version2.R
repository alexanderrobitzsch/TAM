## File Name: tam_jml_version2.R
## File Version: 9.540

tam_jml_version2 <- function( resp, group=NULL, adj=.3, disattenuate=FALSE,
            bias=TRUE, xsi.fixed=NULL,  xsi.inits=NULL, A=NULL, B=NULL, Q=NULL,
            ndim=1, pweights=NULL, control=list(), constraint="cases",
            damp=.1, version=2)
{

    maxiter <- conv <- progress <- tamobj <- convM <- Msteps <- NULL
    R <- NULL
    s11 <- Sys.time()
    # attach control elements
    e1 <- environment()
    con <- list( nodes=seq(-6,6,len=21), snodes=0,
                convD=.001,conv=.0001, convM=.0001, Msteps=10,
                maxiter=1000, cc=TRUE )
    con[ names(control) ] <- control
    Lcon <- length(con)
    con1a <- con1 <- con
    names(con1) <- NULL
    for (cc in 1:Lcon ){
        assign( names(con)[cc], con1[[cc]], envir=e1 )
    }
    resp <- add.colnames.resp(resp)

    # maximum no. of categories per item.
    maxK <- max( resp, na.rm=TRUE ) + 1
    resp <- as.matrix(resp)
    nitems <- ncol(resp)       # number of items
    nstud <- nrow(resp)        # number of students
    nitems <- ncol(resp)       # number of items
    nstud <- nrow(resp)        # number of students

    #-- create design matrices
    design <- designMatrices( modeltype="PCM", maxKi=NULL, resp=resp,
                        A=A, B=B, Q=Q, R=R, ndim=ndim, constraint=constraint )
    A <- design$A
    A.0 <- A
    A.0[ is.na(A.0) ] <- 0
    B <- design$B
    B.0 <- B
    B.0[ is.na(B.0) ] <- 0
    cA <- design$flatA
    cA[is.na(cA)] <- 0
    # number of parameters
    np <- dim(A)[[3]]
    errorP <- rep(0,np)
    AXsi <- matrix(0, nrow=nitems, ncol=maxK)
    if ( is.null( pweights) ){
        pweights <- rep(1,nstud) # weights of response pattern
    }

    # normalize person weights to sum up to nstud
    pweights <- nstud * pweights / sum(pweights)
    # a matrix version of person weights
    pweightsM <- outer( pweights, rep(1,nitems) )

    # xsi inits
    if ( ! is.null(xsi.inits) ){
        xsi <- xsi.inits
    } else {
        xsi <- rep(0,np)
    }
    if ( ! is.null( xsi.fixed ) ){
        xsi[ xsi.fixed[,1] ] <- xsi.fixed[,2]
        est.xsi.index <- setdiff( 1:np, xsi.fixed[,1] )
    } else {
        est.xsi.index <- 1:np
    }


    # group indicators for variance matrix
    if ( ! is.null(group) ){
        groups <- sort(unique(group))
        G <- length(groups)
        if ( length( setdiff( 1:G, groups)  ) > 0 ){
            stop("Label groups from 1, ...,G\n")
        }
    } else {
        G <- 1
    }

    # define response indicator matrix for missings
    resp.ind <- 1 - is.na(resp)
    resp.ind.list <- list( 1:nitems )
    for (i in 1:nitems){ resp.ind.list[[i]] <- which( resp.ind[,i]==1)  }
    resp[ is.na(resp) ] <- 0 # set all missings to zero

    if (version==3){
        m1 <- mean(resp.ind)
        if (m1>.99){
            version <- 2
        }
    }

    # Create an index linking items and parameters
    indexIP <- colSums(aperm(A, c(2,1,3)) !=0, na.rm=TRUE)
    # define list of elements for item parameters
    indexIP.list <- list( 1:np )
    for ( kk in 1:np ){
        indexIP.list[[kk]] <- which( indexIP[,kk] > 0 )
    }

    col.index <- rep( 1:nitems, each=maxK )
    cResp <- resp[, col.index  ]*resp.ind[, col.index ]
    # This line does not take missings into account
    cResp <- 1 * t( t(cResp)==rep(0:(maxK-1), nitems) )
    cResp <- cResp * resp.ind[, col.index ]
    cB <- t( matrix( aperm( B, c(2,1,3) ), nrow=dim(B)[3], byrow=TRUE ) )
    cB[is.na(cB)] <- 0

    # Item sufficient statistics
    ItemScore <- crossprod(cResp %*% cA, pweights )

    # Computer possible maximum parameter score for each person
    maxAi <-  - (apply(-(A), 3, tam_rowMaxs, na.rm=TRUE))
    personMaxA <- resp.ind %*% maxAi
    ItemMax <- crossprod( personMaxA, pweights )

    #Adjust perfect and zero scores for the parameters
    ItemScore[ItemScore==ItemMax] <- ItemScore[ItemScore==ItemMax] + adj
    ItemScore[ItemScore==0] <- ItemScore[ItemScore==0] - adj

    #Initialise xsi with log of odds ratio of raw scores
    xsi[est.xsi.index] <- - log(abs(ItemScore[est.xsi.index]/(ItemMax[est.xsi.index]-ItemScore[est.xsi.index])))

    #Compute person sufficient statistics (total score on each dimension)
    PersonScores <- cResp %*% cB
    # define response pattern
    rp3 <- as.data.frame(tam_01_pattern(x=resp.ind))
    rp3$caseid <- 1:nstud
    rp3$PersonScores <- PersonScores
    rp3$mp.scores <- paste( rp3$mp.index, rp3$PersonScores, sep="-")
    rp3$theta.index <- match( rp3$mp.scores, unique(rp3$mp.scores ) )
    rp3 <- rp3[ order(rp3$theta.index), ]
    rp3.sel <- rp3[ c(TRUE, diff(rp3$theta.index)==1 ), ]
    rp3 <- rp3[ order( rp3$caseid), ]
    rp3.pweightsM  <- rowsum( pweightsM,  rp3$theta.index )

    #Compute possible maximum score for each item on each dimension
    maxBi <- apply(B, 3, rowMaxs, na.rm=TRUE)

    #Compute possible maximum score for each person on each dimension
    PersonMaxB <- resp.ind %*% maxBi

    if ( any(PersonMaxB==0) ){
        stop("Remove persons with only missing item responses!")
    }

    #Adjust perfect scores for each person on each dimension
    PersonScores[PersonScores==PersonMaxB] <- PersonScores[PersonScores==PersonMaxB] - adj

    #Adjust zero scores for each person on each dimension
    PersonScores[PersonScores==0] <- PersonScores[PersonScores==0] + adj

    #Initialise theta (WLE) values for all students
    theta <- log(PersonScores/(PersonMaxB-PersonScores)) #log of odds ratio of raw score

    if (ncol(theta)>1){
        version <- 2
    }

    # center theta?
    center_theta <- is.null(xsi.fixed) & (constraint=="cases")
    nstud3 <- nrow(rp3.sel)

    deviance <- 0
    deviance.history <- matrix( 0, nrow=maxiter, ncol=2)
    colnames(deviance.history) <- c("iter", "deviance")
    deviance.history[,1] <- 1:maxiter

    iter <- 0
    maxthetachange <- meanChangeWLE <- maxChangeWLE <- maxChangeP <- 999    # item parameter change
    # display
    disp <- "....................................................\n"

    #-------------- start iterations -------------------------------
    while ( (( maxthetachange > conv) | (maxChangeP > conv))  & (iter < maxiter) ) {

        xsi_old <- xsi
        iter <- iter + 1
        if (progress){
            cat(disp)
            cat("Iteration", iter, "   ", paste( Sys.time() ) )
            utils::flush.console()
        }
        olddeviance <- deviance

        #update theta, ability estimates
        theta_old <- theta
        jmlAbility <- tam_jml_wle( resp=resp, resp.ind=resp.ind[ rp3.sel$caseid,],
                            A=A, B=B, nstud=nstud3, nitems=nitems, maxK=maxK,
                            convM=convM, PersonScores=PersonScores[ rp3.sel$caseid ],
                            theta=theta[ rp3.sel$caseid,, drop=FALSE],
                            xsi=xsi, Msteps=Msteps, WLE=FALSE, damp=damp, version=version)
        theta <- jmlAbility$theta
        theta <- theta[ rp3$theta.index,, drop=FALSE]
        if (center_theta){
            theta <- theta - mean(theta)
        } else {
            damp <- 1 - (1-damp)*.995
        }
        meanChangeWLE <- jmlAbility$meanChangeWLE
        maxthetachange <- max( abs( theta - theta_old ) )
        errorMLE <- jmlAbility$errorWLE

        # update xsi, item parameters
        jmlxsi <- tam_jml_version2_calc_xsi( resp=resp, resp.ind=resp.ind, A=A,
                        A.0=A.0, B=B, nstud=nstud, nitems=nitems, maxK=maxK, convM=convM,
                        ItemScore=ItemScore, theta=theta, xsi=xsi, Msteps=Msteps, pweightsM=pweightsM,
                        est.xsi.index=est.xsi.index, rp3=rp3, rp3.sel=rp3.sel,
                        rp3.pweightsM=rp3.pweightsM, damp=damp, version=version )
        xsi[est.xsi.index] <- jmlxsi$xsi[est.xsi.index]
        maxChangeP <- jmlxsi$maxChangeP
        errorP[est.xsi.index] <- jmlxsi$errorP[est.xsi.index]

        #-Deviance
        #Calculate Axsi. Only need to do this once for ability estimates.
        for (k in 1:maxK){
            AXsi[, k ] <- A[,k, ] %*% xsi
        }
        theta.unique <- unique(theta)
        nstud1 <- length(theta.unique)
        if (version %in% c(2,3) ){
            res <- tam_mml_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi, B=B, xsi=xsi,
                    theta=theta.unique, nnodes=nstud1, maxK=maxK, recalc=FALSE )
            rprobs <- res$rprobs
        }
        #-- inactive code
        if (version==-99){
            res <- tam_rcpp_tam_jml_calc_probs(theta=as.vector(theta.unique), AXsi=AXsi,
                        B=B[,,1], maxK=maxK, resp_ind=resp.ind )
            nstud1 <- length(theta.unique)
            rprobs <- array(res$probs, dim=c(nitems,maxK,nstud1))
        }


        crprobs <- t( matrix( aperm( rprobs, c(2,1,3) ), nrow=dim(rprobs)[3], byrow=TRUE ) )
        crprobs <- crprobs[, match( theta, theta.unique) ]
        cr <- log(crprobs) * t(cResp)
        deviance <- -2 * sum(cr, na.rm=TRUE)
        deviance.history[iter,2] <- deviance
        # progress bar
        if (progress){
            cat( paste( "\n  Deviance=", round( deviance, 4 ) ))
            if (iter>1){ cat( " | Deviance change:", -round( deviance-olddeviance, 4 ) ) }
            cat( "\n  Maximum MLE/WLE change:", round( maxthetachange, 6 ) )
            cat( "\n  Maximum item parameter change:", round( maxChangeP, 6 ) )
            cat( "\n" )
            utils::flush.console()
        }
        if ( abs( deviance-olddeviance) < 1e-10 ){ break }
    }
    #--------------------- end iterations ---------

    s1 <- Sys.time()
    #After convergence, compute final WLE (WLE set to TRUE)
    jmlWLE <- tam_jml_wle( tamobj=tamobj, resp=resp, resp.ind=resp.ind[ rp3.sel$caseid,],
                            A=A, B=B, nstud=nrow(rp3.sel), nitems=nitems, maxK=maxK,
                            convM=convM, PersonScores=PersonScores[ rp3.sel$caseid ],
                            theta=theta[ rp3.sel$caseid,, drop=FALSE], xsi=xsi,
                            Msteps=Msteps, WLE=TRUE, version=version)
    thetaWLE <- jmlWLE$theta[rp3$theta.index,1]
    meanChangeWLE <- jmlWLE$meanChangeWLE
    errorWLE <- jmlWLE$errorWLE[rp3$theta.index]

    #WLE person separation reliability
    WLEreliability <- WLErel(theta=thetaWLE, error=errorWLE, w=pweights)

    #disattenuate
    if (disattenuate){
        thetaWLE <- sqrt(WLEreliability) * thetaWLE
        theta <- sqrt(WLEreliability) * theta
    }
    #bias
    if (bias){
        xsi[est.xsi.index] <- (nitems - 1)/nitems * xsi[est.xsi.index]     #Check this for more complex models
    }

    # collect item statistics
    item <- data.frame( xsi.label=dimnames(A)[[3]], xsi.index=1:(length(xsi)),
                            xsi=xsi, se.xsi=errorP)

    #** response probabilities
    rprobs <- rprobs[,, match( theta, theta.unique)]

    #- computation time
    s2 <- Sys.time()
    if (progress){
        cat(disp)
        cat( "\nStart: ", paste(s11))
        cat( "\nEnd: ", paste(s2),"\n")
        print(s2-s11)
        cat( "\n" )
    }

    #--- Output list
    deviance.history <- deviance.history[ 1:iter, ]
    res <- list( item=item, xsi=xsi, errorP=errorP, theta=theta[,1], errorWLE=errorWLE,
                    WLE=thetaWLE, WLEreliability=WLEreliability, PersonScores=PersonScores,
                    ItemScore=ItemScore, PersonMax=PersonMaxB, ItemMax=ItemMax,
                    deviance=deviance, deviance.history=deviance.history,
                    resp=resp, resp.ind=resp.ind, group=group, pweights=pweights,
                    A=A, B=B, nitems=nitems, maxK=maxK, rprobs=rprobs, nstud=nstud,
                    resp.ind.list=resp.ind.list, xsi.fixed=xsi.fixed, deviance=deviance,
                    deviance.history=deviance.history, control=con1a, iter=iter,
                    version=version)
    res$time <-  c(s11,s2)
    class(res) <- "tam.jml"
    return(res)
}


# cat("\n WLE \n"); s2 <- Sys.time(); print(s2-s1) ; s1 <- s2
