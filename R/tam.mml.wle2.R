## File Name: tam.mml.wle2.R
## File Version: 0.38
## File Last Change: 2017-09-15 17:31:34
################################################################
################################################################
################################################################
tam.mml.wle2 <-
  function( tamobj, score.resp=NULL , WLE=TRUE , adj=.3 , Msteps=20 , 
            convM = .0001 , progress=TRUE , output.prob=FALSE ){
    #########################################################
    # INPUT:
    # tamobj ... result from tam analysis
    # (WLE = TRUE) will produce WLE. Otherwise it will be MLE
    # 
    #########################################################
    #  adj <- 0.3
    #  Msteps <- 20
    #  convM <- .0001
	CALL <- match.call()

	#--- process input data
	res <- tam_mml_wle_proc_input_data( tamobj=tamobj, score.resp=score.resp ) 
	AXsi <- res$AXsi
	B <- res$B
	resp <- res$resp
	resp.ind <- res$resp.ind
	nitems <- res$nitems
	nstud <- res$nstud
	ndim <- res$ndim
	maxK <- res$maxK
	pweights <- res$pweights
	pid <- res$pid
	A <- res$A
	xsi <- res$xsi

	#*** readjust in case of WLE
	if (WLE){
		adj <- 0
	}
	
    col.index <- rep( 1:nitems , each = maxK )
    cResp <- resp[ , col.index  ]*resp.ind[ , col.index ]
    cResp <- 1 * t( t(cResp) == rep(0:(maxK-1), nitems) )
    cB <- t( matrix( aperm( B , c(2,1,3) ) , nrow = dim(B)[3] , byrow = TRUE ) )
    cB[is.na(cB)] <- 0
    #Compute person sufficient statistics (total score on each dimension)
    PersonScores <- cResp %*% cB
    
    #Compute possible maximum score for each item on each dimension
    maxBi <- apply(B , 3 , tam_rowMaxs , na.rm = TRUE)
    
    #Compute possible maximum score for each person on each dimension
    PersonMax <- resp.ind %*% maxBi
    PersonMax[ PersonMax == 0 ] <- 2 * adj
    
    #Adjust perfect scores for each person on each dimension
    PersonScores[PersonScores==PersonMax] <- PersonScores[PersonScores==PersonMax] - adj
    
    #Adjust zero scores for each person on each dimension
    PersonScores[PersonScores==0] <- PersonScores[PersonScores==0] + adj
    
    #Calculate Axsi. Only need to do this once.
    # for (i in 1:nitems) {
    #  for (k in 1:maxK){
    #    AXsi[i,k] <- ( A[i,k,] %*% xsi )
    #  }
    # }
    
    #Initialise theta (WLE) values for all students
    theta <- log((PersonScores+.5)/(PersonMax-PersonScores+1)) #log of odds ratio of raw score
    
    ######################################
    #Compute WLE
    #similar to the M step in the tam function, but each student's theta vector is now one node.
    converge <- FALSE
    Miter <- 0
    BB <- array (0, dim=c(nitems,maxK,ndim,ndim))
    BBB <- array (0, dim=c(nitems,maxK,ndim)) 
    for (i in 1:nitems) {
      for (k in 1:maxK) {
        BB[i,k,,] <- B[i,k,] %*% t(B[i,k,])
        BBB[i,k,] <- BB[i,k,,] %*% B[i,k,]
      }
    }
    BL <- matrix(B, nitems*maxK, ndim)
    BBL <- matrix(BB, nitems*maxK, ndim*ndim)
    BBBL <- matrix(BBB, nitems*maxK, ndim)
    
    increment <- array(0, dim=c(nstud,ndim))
    old_increment <- 3 + increment
    
    
    # Begin iterations
    while (!converge & ( Miter <= Msteps ) ) {
      
      resWLE <- calc_prob.v5(iIndex = 1:nitems , A , AXsi , 
                             B , xsi , theta , nstud, maxK , recalc=FALSE )      	
      rprobsWLE <- resWLE$rprobs
      rprobsWLEL <- matrix(rprobsWLE, nitems*maxK, nstud )
      
      rprobsWLEL[is.na(rprobsWLEL)] <- 0
      resB <- tam_wle_Bs( rprobsWLEL, resp.ind, BL, BBL, BBBL, 
                        ndim, nitems, maxK, nstud )
      B_bari <- array(resB$B_bari, dim=c(nstud, nitems,ndim))
      BB_bari <- array(resB$BB_bari, dim=c(nstud, nitems, ndim, ndim))
      BBB_bari <- array(resB$BBB_bari, dim=c(nstud, nitems, ndim))
      
      B_Sq <- array(resB$B_Sq,dim=c(nstud, nitems, ndim, ndim))
      B2_B <- array(resB$B2_B,dim=c(nstud, nitems, ndim))
      B_Cube <- array(resB$B_Cube,dim=c(nstud, nitems, ndim))
       expected <- colSums(aperm(B_bari,c(2,1,3)))
      err <- colSums(aperm(BB_bari,c(2,1,3,4))) - colSums(aperm(B_Sq, c(2,1,3,4)))  #sum over the items
   
      if (ndim == 1) {
        # err_inv <- apply(err,1,function(x) 1/x )
        err_inv <- 1 / err
      } else {
        ## diag err_i= forall i
        diag_ind <- cbind(rep(1:nstud, each=ndim), 1:ndim, 1:ndim)
        err[diag_ind] <- err[diag_ind]+1E-15    
        errl <- matrix(err, nstud, ndim*ndim)
      
        err_inv <- tam_wle_errinv( errl, ndim, nstud )
      }
        
      err_inv <- array(abs(err_inv),dim=c(nstud,ndim,ndim))
      warm <- -3*B2_B + 2*B_Cube + BBB_bari
      warmadd <- colSums(aperm(warm,c(2,1,3)))  #sum over the items
      scores <- PersonScores - expected
      if (WLE) {
        warmaddon <- array(0,dim=c(nstud,ndim))
        for (d1 in 1:ndim) {
          warmaddon[,d1] <- 0
          for (d2 in 1:ndim) {
            warmaddon[,d1] <- warmaddon[,d1] + err_inv[,d1,d2]*warmadd[,d2]
          }
        }
        scores <- scores + warmaddon/2.0      
      }
      
      increment <- array(0, dim=c(nstud,ndim))
      for (d1 in 1:ndim) {
        increment[,d1] <- 0
        for (d2 in 1:ndim) {
          increment[,d1] <- increment[,d1] + err_inv[,d1,d2]*scores[,d2]
        }
      }
      
      # dampening the increment
      for ( d1 in 1:ndim){ 
        #	   increment[,d1] <- ifelse( abs(increment[,d1]) > 3 , sign( increment[,d1] )*3 , increment[,d1] )
        ci <- ceiling( abs(increment[,d1]) / ( abs( old_increment[,d1]) + 10^(-10) ) )	   
        increment[,d1] <- ifelse( abs( increment[,d1]) > abs(old_increment[,d1])  , 
                                  increment[,d1]/(2*ci) , 
                                  increment[,d1] )	   
#        old_increment[,d1] <- increment[,d1] 
		old_increment[,d1] <- .95 * old_increment[,d1]
        #***
        # avoid NaNs in increment
        increment[,d1] <- ifelse( is.na(increment[,d1] ) , 0 , increment[,d1] )
        # increment[abs(increment)>3] <- sign(increment[abs(increment)>3])*3	
      }
      
      theta <- theta + increment
      if ( max(abs(increment)) < convM ) {
        converge <- TRUE
      }
      
      Miter <- Miter + 1 

      if (progress){
		  cat( paste( "Iteration in WLE/MLE estimation ", Miter, 
					  "  | Maximal change " , round( max(abs(increment)) , 4) , "\n" )  ) 
		  utils::flush.console()
					}
    }  # end of Newton-Raphson


	res <- tam_mml_wle_postproc( ndim=ndim, err_inv=err_inv, theta=theta, pid=pid, 
				resp.ind=resp.ind, PersonScores=PersonScores, PersonMax=PersonMax, 
				adj=adj, WLE=WLE, rprobsWLE=rprobsWLE, output.prob=output.prob, progress=progress, 
				pweights=pweights, CALL=CALL, B=B )
	
    #  res <- list( "PersonScores" = PersonScores, "PersonMax" = PersonMax, "theta" = theta , "error" =  error )
    return(res)
  }
