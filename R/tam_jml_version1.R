## File Name: tam_jml_version1.R
## File Version: 9.369

tam_jml_version1 <- function( resp, group=NULL, adj=.3, disattenuate=FALSE,
                     bias=TRUE, xsi.fixed=NULL,  xsi.inits=NULL,
                     theta.fixed=NULL,
                     A=NULL, B=NULL, Q=NULL, ndim=1,
                     pweights=NULL, theta_proc=NULL, control=list() )
{


  maxiter <- conv <- progress <- tamobj <- convM <- Msteps <- NULL
  R <- NULL

#  adj <- 0.3   # adjustment for perfect and zero scores
  s1 <- Sys.time()
  # attach control elements
  e1 <- environment()
  con <- list( nodes=seq(-6,6,len=21), snodes=0,
               convD=.001,conv=.0001, convM=.0001, Msteps=10,
               maxiter=1000, progress=TRUE )
  con[ names(control) ] <- control
  Lcon <- length(con)
  con1a <- con1 <- con ;
  names(con1) <- NULL
  for (cc in 1L:Lcon ){
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
  ################################
  # create design matrices

  design <- designMatrices( modeltype="PCM", maxKi=NULL, resp=resp,
                            A=A, B=B, Q=Q, R=R, ndim=ndim )
  A <- design$A
  B <- design$B
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
  } else { xsi <- rep(0,np)   }
  if ( ! is.null( xsi.fixed ) ){
    xsi[ xsi.fixed[,1] ] <- xsi.fixed[,2]
    est.xsi.index <- setdiff( 1L:np, xsi.fixed[,1] )
  } else { est.xsi.index <- 1L:np }


  # group indicators for variance matrix
  if ( ! is.null(group) ){
    groups <- sort(unique(group))
    G <- length(groups)
    # user must label groups from 1, ..., G
    if ( length( setdiff( 1L:G, groups)  ) > 0 ){
      stop("Label groups from 1, ...,G\n")
    }
  } else { G <- 1 }

  # define response indicator matrix for missings
  resp.ind <- 1 - is.na(resp)
  resp.ind.list <- list( 1L:nitems )
  for (i in 1L:nitems){ resp.ind.list[[i]] <- which( resp.ind[,i]==1)  }
  resp[ is.na(resp) ] <- 0     # set all missings to zero

  # Create an index linking items and parameters
  indexIP <- colSums(aperm(A, c(2,1,3)) !=0, na.rm=TRUE)
  # define list of elements for item parameters
  indexIP.list <- list( 1L:np )
  for ( kk in 1L:np ){
    indexIP.list[[kk]] <- which( indexIP[,kk] > 0 )
  }


  col.index <- rep( 1L:nitems, each=maxK )
  cResp <- resp[, col.index  ]*resp.ind[, col.index ]
  # This line does not take missings into account
  cResp <- 1 * t( t(cResp)==rep(0:(maxK-1), nitems) )
  ##@@@##
  # ARb: I added this line
  cResp <- cResp * resp.ind[, col.index ]
  cB <- t( matrix( aperm( B, c(2,1,3) ), nrow=dim(B)[3], byrow=TRUE ) )
  cB[is.na(cB)] <- 0

  # Item sufficient statistics
  ItemScore <- crossprod(cResp %*% cA, pweights )

  # Computer possible maximum parameter score for each person
  maxAi <-  - (apply(-(A), 3, tam_rowMaxs, na.rm=TRUE))
  personMaxA <- resp.ind %*% maxAi
  # ItemMax <- personMaxA %t*% pweights
  ItemMax <- crossprod( personMaxA, pweights )

  #Adjust perfect and zero scores for the parameters
  ItemScore[ItemScore==ItemMax] <- ItemScore[ItemScore==ItemMax] + adj #..."+" sign, because ItemScore is -ve)
  ItemScore[ItemScore==0] <- ItemScore[ItemScore==0] - adj

  #Initialise xsi
  xsi[est.xsi.index] <- - log(abs(ItemScore[est.xsi.index]/(ItemMax[est.xsi.index]-ItemScore[est.xsi.index])))  #log of odds ratio of raw scores

  #Compute person sufficient statistics (total score on each dimension)
  PersonScores <- cResp %*% cB

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
  if ( ! is.null( theta.fixed ) ){
    theta[ theta.fixed[,1] ] <- theta.fixed[,2]
                            }


  deviance <- 0
  deviance.history <- matrix( 0, nrow=maxiter, ncol=2)
  colnames(deviance.history) <- c("iter", "deviance")
  deviance.history[,1] <- 1L:maxiter

  iter <- 0
  meanChangeWLE <- maxChangeWLE <- maxChangeP <- 999    # item parameter change
  # display
  disp <- "....................................................\n"


  ##############################################################
  #Start convergence loop here
  while ( ((meanChangeWLE > conv) | (maxChangeP > conv))  & (iter < maxiter) ) {

    iter <- iter + 1
    if (progress){
      cat(disp)
      cat("Iteration", iter, "   ", paste( Sys.time() ) )
      cat( "\n" )
      flush.console()
    }
    olddeviance <- deviance

    #-- update theta (ability estimates)
    jmlAbility <- tam_jml_wle( tamobj, resp, resp.ind, A, B, nstud, nitems, maxK, convM,
                    PersonScores, theta, xsi, Msteps, WLE=FALSE,
                    theta.fixed=theta.fixed, theta_proc=theta_proc)

    theta <- jmlAbility$theta
    if (is.null(xsi.fixed)){
        theta <- theta - mean(theta)
    }
    meanChangeWLE <- jmlAbility$meanChangeWLE
    errorMLE <- jmlAbility$errorWLE


    #update xsi, item parameters
    jmlxsi <- tam_jml_version1_calc_xsi( resp, resp.ind, A, B, nstud, nitems, maxK, convM,
                            ItemScore, theta, xsi, Msteps, pweightsM,
                            est.xsi.index)
    xsi[est.xsi.index] <- jmlxsi$xsi[est.xsi.index]
    maxChangeP <- jmlxsi$maxChangeP
    errorP[est.xsi.index] <- jmlxsi$errorP[est.xsi.index]

    #Deviance
    #Calculate Axsi. Only need to do this once for ability estimates.
    for (i in 1L:nitems) {
      for (k in 1L:maxK){
        AXsi[i,k] <- ( A[i,k,] %*% xsi )
      }
    }
    res <- tam_mml_calc_prob(iIndex=1L:nitems, A, AXsi,
                        B, xsi, theta, nstud, maxK, recalc=FALSE )
    rprobs <- res[["rprobs"]]
    crprobs <- t( matrix( aperm( rprobs, c(2,1,3) ), nrow=dim(rprobs)[3], byrow=TRUE ) )
    cr <- crprobs * t(cResp)
    cr <- cr[cr>0]
    deviance <- -2 * sum(log(cr), na.rm=TRUE)
    deviance.history[iter,2] <- deviance

    # progress bar
    if (progress){
      cat( paste( "\n  Deviance=", round( deviance, 4 ) ))
      if (iter>1){ cat( " | Deviance change:", -round( deviance-olddeviance, 4 ) ) }
      cat( "\n  Mean WLE change:", round( meanChangeWLE, 6 ) )
      cat( "\n  Maximum parameter change:", round( maxChangeP, 6 ) )
      cat( "\n" )
      flush.console()
    }
    #@ARb 2012-08-27
    # stop loop (break) if there is no change in deviation
    if ( abs( deviance-olddeviance) < 1E-10 ){ break }

  }# end of all convergence

  #After convergence, compute final WLE (WLE set to TRUE)

  jmlWLE <- tam_jml_wle( tamobj, resp, resp.ind, A, B, nstud, nitems, maxK, convM,
                          PersonScores, theta, xsi, Msteps, WLE=TRUE,
                          theta.fixed=theta.fixed, theta_proc=theta_proc )

  thetaWLE <- jmlWLE$theta[,1]

  meanChangeWLE <- jmlWLE$meanChangeWLE
  errorWLE <- jmlWLE$errorWLE

  #WLE person separation reliability
  varWLE <- stats::var(thetaWLE)
  WLEreliability <- (varWLE - mean(errorWLE^2)) / varWLE

  #disattenuate
  if (disattenuate) {
    thetaWLE <- sqrt(WLEreliability) * thetaWLE
    theta <- sqrt(WLEreliability) * theta
  }

  #bias
  if (bias) {
    xsi[est.xsi.index] <- (nitems - 1)/nitems * xsi[est.xsi.index]     #Check this for more complex models
  }


  # collect item statistics
  item <- data.frame( "xsi.label"=dimnames(A)[[3]],
        "xsi.index"=1L:( length(xsi) ), "xsi"=xsi,
        "se.xsi"=errorP
    )



  ############################################################
  s2 <- Sys.time()
  if (progress){
    cat(disp)
    cat( "\nStart: ", paste(s1))
    cat( "\nEnd: ", paste(s2),"\n")
    print(s2-s1)
    cat( "\n" )
  }

  # Output list
  deviance.history <- deviance.history[ 1L:iter, ]
  res <- list( "item"=item, "xsi"=xsi,  "errorP"=errorP,
               "theta"=theta[,1], "errorWLE"=errorWLE,  "WLE"=thetaWLE,
               "WLEreliability"=WLEreliability,
               "PersonScores"=PersonScores, "ItemScore"=ItemScore,
               "PersonMax"=PersonMaxB, "ItemMax"=ItemMax,
               "deviance"=deviance, "deviance.history"=deviance.history,
               "resp"=resp, "resp.ind"=resp.ind, "group"=group,
               "pweights"=pweights, "A"=A, "B"=B, AXsi=AXsi,
               "nitems"=nitems, "maxK"=maxK,
               "nstud"=nstud, "resp.ind.list"=resp.ind.list,
               "xsi.fixed"=xsi.fixed, "deviance"=deviance,
               "deviance.history"=deviance.history,
               "control"=con1a, "iter"=iter)
  res$time <-  c(s1,s2)
  class(res) <- "tam.jml"
  return(res)
}
