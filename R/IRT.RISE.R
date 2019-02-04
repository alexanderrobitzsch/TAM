## File Name: IRT.RISE.R
## File Version: 0.157

## RISE item fit statistic for comparing the fit of
## a parametric and a nonparametric model
IRT.RISE <- function( mod_p, mod_np, use_probs=TRUE )
{
    # first model
    p1 <- IRT.irfprob(mod_p)
    probs <- aperm( p1, perm=c(3,1,2) )
    pi.k <- attr(p1, "prob.theta")
    # second model
    n.ik <- IRT.expectedCounts(mod_np)
    n.ik <- aperm(n.ik, perm=c(3,1,2,4))
    p2 <- aperm( IRT.irfprob(mod_np), perm=c(3,1,2) )
    if (use_probs){
        n.ik[,,,1] <- p2
    }
    fmod <- CDM::IRT_RMSD_calc_rmsd( n.ik=n.ik, pi.k=pi.k, probs=probs )
    res <- fmod$RMSD
    return(res)
}
