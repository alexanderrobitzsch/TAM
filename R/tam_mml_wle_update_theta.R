## File Name: tam_mml_wle_update_theta.R
## File Version: 0.03

tam_mml_wle_update_theta <- function( theta, PersonScores, err_inv, nstud, ndim,
        B2_B, B_Cube, BBB_bari, expected, WLE, Miter, progress, convM,
        old_increment, converge, error)
{

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
        increment[,d1] <- tam_trim_increment(increment = increment[,d1] ,
                                max.increment = abs(old_increment[,d1]),
                                trim_increment = "half", avoid_na = TRUE )
        old_increment[,d1] <- 0.95*increment[,d1]
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
    #-----------
    # OUTPUT
    res <- list( theta=theta, increment=increment, scores=scores, converge=converge,
                    old_increment=old_increment, err_inv=err_inv, Miter = Miter)
    return(res)
}
