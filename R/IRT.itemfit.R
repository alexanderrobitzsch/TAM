## File Name: IRT.itemfit.R
## File Version: 9.091

###########################################################################
IRT_itemfit_rmsea_default <- function(object)
{
    mod1 <- object
    probs <- IRT.irfprob(mod1)
    n.ik <- IRT.expectedCounts(mod1)
    pi.k <- attr(probs, "prob.theta")
    if ( is.vector(pi.k) ){
        pi.k <- matrix( pi.k, ncol=1 )
    }
    n.ik <- aperm( n.ik, c(3,1,2,4))
    probs <- aperm( probs, c(3,1,2))
    res <- itemfit.rmsea( n.ik=n.ik, pi.k=pi.k, probs=probs )
    return(res)
}
###########################################################################
IRT.itemfit.tam.default <- function(object, method="RMSD", ... )
{
    res <- NULL
    if ( method %in% c("RMSD","rmsea") ){
        # res <- IRT.itemfit.rmsea.default( object=object )
        res <- CDM::IRT.RMSD(object=object)
    }
    return(res)
}
############################################################################
IRT.itemfit.tam.mml <- IRT.itemfit.tam.default
IRT.itemfit.tam.mml.2pl <- IRT.itemfit.tam.default
IRT.itemfit.tam.mml.mfr <- IRT.itemfit.tam.default
IRT.itemfit.tam.mml.3pl <- IRT.itemfit.tam.default
##############################################################################
