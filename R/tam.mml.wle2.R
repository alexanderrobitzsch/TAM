

tam.mml.wle2 <- function( tamobj, score.resp=NULL , WLE=TRUE , adj=.3 , Msteps=20 , 
            convM = .0001 , progress=TRUE , output.prob=FALSE )
{
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
	
	#--- initial values and some design matrices
	res <- tam_mml_wle_theta_inits( WLE=WLE, adj=adj, nitems=nitems, maxK=maxK, 
				resp=resp, resp.ind=resp.ind, B=B, ndim=ndim ) 
	adj <- res$adj
	PersonScores <- res$PersonScores
	PersonMax <- res$PersonMax
	theta <- res$theta
	converge <- res$converge
	Miter <- res$Miter
	BB <- res$BB
	BBB <- res$BBB
	increment <- res$increment
	old_increment <- res$old_increment

	#--- create objects needed in iterations
    BL <- matrix(B, nitems*maxK, ndim)
    BBL <- matrix(BB, nitems*maxK, ndim*ndim)
    BBBL <- matrix(BBB, nitems*maxK, ndim)    
    
    # Begin iterations
    while (!converge & ( Miter <= Msteps ) ) {      
		resWLE <- tam_mml_calc_prob( iIndex=1:nitems, A=NULL, AXsi=AXsi, B=B, xsi=NULL, 
						theta=theta, nnodes=nstud, maxK=maxK, recalc=FALSE ) 							 							 
		rprobsWLE <- resWLE$rprobs
		rprobsWLEL <- matrix(rprobsWLE, nitems*maxK, nstud )
      
		rprobsWLEL[is.na(rprobsWLEL)] <- 0
		resB <- tam_wle_Bs( RPROBS=rprobsWLEL, RESPIND=resp.ind, CBL=BL, 
							CBB=BBL, CBBB=BBBL, cndim=ndim, cnitems=nitems, 
							cmaxK=maxK, cnstud=nstud )
		B_bari <- array(resB$B_bari, dim=c(nstud, nitems,ndim))
		BB_bari <- array(resB$BB_bari, dim=c(nstud, nitems, ndim, ndim))
		BBB_bari <- array(resB$BBB_bari, dim=c(nstud, nitems, ndim))
      
		B_Sq <- array(resB$B_Sq,dim=c(nstud, nitems, ndim, ndim))
		B2_B <- array(resB$B2_B,dim=c(nstud, nitems, ndim))
		B_Cube <- array(resB$B_Cube,dim=c(nstud, nitems, ndim))
		expected <- colSums(aperm(B_bari,c(2,1,3)))
		err <- colSums(aperm(BB_bari,c(2,1,3,4))) - colSums(aperm(B_Sq, c(2,1,3,4)))  #sum over the items
   
		if (ndim == 1) {
			err_inv <- 1 / err
		} else {
			diag_ind <- cbind(rep(1:nstud, each=ndim), 1:ndim, 1:ndim)
			err[diag_ind] <- err[diag_ind]+1E-15    
			errl <- matrix(err, nstud, ndim*ndim)
			err_inv <- tam_wle_errinv( myERR=errl, cndim=ndim, cnstud=nstud )
		}
        
		#--- update increments and theta
		res <- tam_mml_wle_update_theta( theta=theta, PersonScores=PersonScores, 
					err_inv=err_inv, nstud=nstud, ndim=ndim, B2_B=B2_B, B_Cube=B_Cube, 
					BBB_bari=BBB_bari, expected=expected, WLE=WLE, Miter=Miter, progress=progress, 
					convM=convM, old_increment=old_increment, converge=converge ) 
		theta <- res$theta
		increment <- res$increment
		scores <- res$scores
		converge <- res$converge
		old_increment <- res$old_increment
		err_inv <- res$err_inv
		Miter <- res$Miter
		
    }  # end of Newton-Raphson
	#----------------------------------------
	
	res <- tam_mml_wle_postproc( ndim=ndim, err_inv=err_inv, theta=theta, pid=pid, 
				resp.ind=resp.ind, PersonScores=PersonScores, PersonMax=PersonMax, 
				adj=adj, WLE=WLE, rprobsWLE=rprobsWLE, output.prob=output.prob, progress=progress, 
				pweights=pweights, CALL=CALL, B=B ) 	
    return(res)
}

