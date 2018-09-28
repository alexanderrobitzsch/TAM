## File Name: tam.mml.wle.R
## File Version: 0.26


tam.mml.wle <- function( tamobj, score.resp=NULL, WLE=TRUE, adj=.3, Msteps=20,
            convM=.0001, progress=TRUE,    output.prob=FALSE )
{
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

    #----------------------------------
    #----- begin iterations
    while (!converge & ( Miter <=Msteps ) ) {
        resWLE <- tam_mml_calc_prob( iIndex=1:nitems, A=NULL, AXsi=AXsi, B=B, xsi=NULL,
                        theta=theta, nnodes=nstud, maxK=maxK, recalc=FALSE )
        rprobsWLE <- resWLE$rprobs
        B_bari <- array(0,dim=c(nstud, nitems, ndim))
        BB_bari <- array(0, dim=c(nstud, nitems, ndim, ndim))
        BBB_bari <- array(0,dim=c(nstud, nitems, ndim))
        for (d1 in 1:ndim) {
            B_bari[,,d1] <- sapply(1:nitems, function(i) colSums(B[i,,d1] * rprobsWLE[i,,], na.rm=TRUE)) * resp.ind
            for (d2 in 1:ndim) {
                BB_bari[,,d1,d2] <- sapply(1:nitems, function(i) colSums(BB[i,,d1,d2] * rprobsWLE[i,,], na.rm=TRUE)) *resp.ind
            }
            BBB_bari[,,d1] <- sapply(1:nitems, function(i) colSums(BBB[i,,d1] * rprobsWLE[i,,], na.rm=TRUE)) *resp.ind
        }

        B_Sq <- array(0,dim=c(nstud, nitems, ndim, ndim))
        B2_B <- array(0,dim=c(nstud, nitems, ndim))
        B_Cube <- array(0,dim=c(nstud, nitems, ndim))
        for (d1 in 1:ndim) {
            B2_B[,,d1] <- 0
            B_Cube[,,d1] <- 0
            for (d2 in 1:ndim) {
                B_Sq[,,d1,d2] <- B_bari[,,d1]*B_bari[,,d2]
                B2_B[,,d1] <- B2_B[,,d1] + BB_bari[,,d1,d2]*B_bari[,,d2]
                B_Cube[,,d1] <- B_Cube[,,d1] + B_Sq[,,d1,d2]*B_bari[,,d2]
            }
        }
        expected <- colSums(aperm(B_bari,c(2,1,3)))
        err <- colSums(aperm(BB_bari,c(2,1,3,4))) - colSums(aperm(B_Sq, c(2,1,3,4)))  #sum over the items
        if (ndim==1) {
            err_inv <- 1 / err
        } else {
            err_inv <- aperm(apply(err,1,function(ee){
                tam_solve_ridge(ee, ridge=1E-15)
                }
            ),c(2,1))
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
    #---------------------------------------

    res <- tam_mml_wle_postproc( ndim=ndim, err_inv=err_inv, theta=theta, pid=pid,
                resp.ind=resp.ind, PersonScores=PersonScores, PersonMax=PersonMax,
                adj=adj, WLE=WLE, rprobsWLE=rprobsWLE, output.prob=output.prob, progress=progress,
                pweights=pweights, CALL=CALL, B=B )
    return(res)
}

