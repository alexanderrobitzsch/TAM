## File Name: tam_jml_version2_calc_xsi.R
## File Version: 9.55

tam_jml_version2_calc_xsi <- function ( resp, resp.ind, A, A.0, B, nstud, nitems,
            maxK, convM, ItemScore, theta, xsi, Msteps, pweightsM,
            est.xsi.index, rp3, rp3.sel, rp3.pweightsM )
{

    #Update item parameters
    AXsi <- matrix(0, nrow=nitems, ncol=maxK)
    r <- matrix(0,nrow=nitems,ncol=maxK)
    rr <- array(0,dim=c(nitems,maxK,maxK))
    AA <- array (0, dim=c(nitems,maxK,maxK))
    eps <- 1E-10
    maxChangeP <- 0
    errorP <- rep(0, max(est.xsi.index))
    convergeAllP <- FALSE
    p_loop <- est.xsi.index

    PP <- length(p_loop)
    old_xsi <- xsi
    PP1 <- length(xsi)
    convergeP <- rep(FALSE,PP1)
    nonest.xsi.index <- setdiff( seq(1,PP1), est.xsi.index )
    convergeP[ nonest.xsi.index ] <- TRUE

    # begin loop
    iterP <- 1
    old_increment <- rep(5,PP1)
    cat(" Item parameter estimation |")
    while (!convergeAllP & ( iterP <=Msteps ) ) {
        res.p <- tam_mml_calc_prob( iIndex=1:nitems, A=A, AXsi=AXsi,
                                B=B, xsi=xsi, theta=theta[ rp3.sel$caseid,,drop=FALSE ],
                                nnodes=nrow(rp3.sel), maxK=maxK, recalc=TRUE )
        rprobs <- res.p$rprobs
        M1 <- resp.ind[ rp3.sel$caseid, ] * rp3.pweightsM
        t_rprobs <- aperm( rprobs, dim=c(3,2,1) )
        for (k1 in 1:maxK) {
            M1_k1 <- t_rprobs[,k1,] * M1
            r[,k1] <- colSums( M1_k1, na.rm=TRUE)
            for (k2 in 1:maxK) {
                rr[,k1,k2] <- colSums( M1_k1 * t_rprobs[,k2,], na.rm=TRUE)
            }
        }

        A_Sq <- AA_bari <- A_bari <- matrix( 0, PP1, nitems )
        t_r <- t(r)
        t_rr <- aperm( rr, dim=c(3,2,1) )
        t_A.0 <- aperm( A.0, dim=c(3,2,1) )
        for (kk in 1:maxK){
            A0_r_kk <- A.0[, kk, ] * r[, kk ]
            A_bari <- A_bari + t( A0_r_kk )
            AA_bari <- AA_bari + t( A.0[, kk, ] * A0_r_kk )
        }
        for (kk1 in 1:maxK){
            for (kk2 in 1:maxK){
              fac <- 1
              # A_Sq <- A_Sq + fac * t( A.0[,kk1,] * A.0[,kk2,] * rr[, kk1, kk2 ] )
                if ( kk1 < kk2 ){ fac <- 2 }
                if (kk1 <=kk2 ){
                    A_Sq <- A_Sq + fac * t( A.0[,kk1,] * A.0[,kk2,] * rr[, kk1, kk2 ] )
                }
            }
        }

        expected <- rowSums(A_bari, na.rm=TRUE) # sum over items
        err <- rowSums(AA_bari - A_Sq, na.rm=TRUE)   #sum over the items

        err_inv <- abs (1/( abs(err) + eps ))
        scores <- ItemScore * ( ! convergeP ) - expected
        increment <-  err_inv*scores
        increment <- tam_trim_increment(increment=increment, max.increment=abs(old_increment),
                            trim_increment="half", trim_incr_factor=2, eps=eps)
        increment[ nonest.xsi.index ] <- 0
        xsi <- xsi + increment
        old_increment <- increment
        errorP <- sqrt(err_inv)
        convergeP[ abs(increment) < convM ] <- TRUE
        utils::flush.console()
        iterP <- iterP + 1
        p_loop <- est.xsi.index[convergeP[est.xsi.index]==FALSE]
        convergeAllP <- (sum(convergeP[est.xsi.index])==length(est.xsi.index))
        cat("-")
    } # end of all parameters convergence

    #--- OUTPUT
    res <- list( xsi=xsi, errorP=errorP, maxChangeP=max(abs(xsi-old_xsi)))
    return(res)
}


tam.jml.xsi2 <- tam_jml_version2_calc_xsi

# cat("one iteration xsi -- matrix 1 (V2)") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
