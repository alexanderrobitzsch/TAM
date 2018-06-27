## File Name: IRT.RISE.R
## File Version: 0.03

## RISE item fit statistic for comparing the fit of
## a parametric and a nonparametric model
IRT.RISE <- function( mod_p, mod_np )
{
    p1 <- IRT.irfprob(mod_p)
    probs <- aperm( p1, perm=c(3,1,2) )
    pi.k <- attr(p1, "prob.theta")
    n.ik <- IRT.expectedCounts( mod_np )
    fmod <- CDM::IRT_RMSD_calc_rmsd( n.ik=n.ik, pi.k=pi.k, probs=probs )    
    res <- fmod$RMSD
    return(res)
}
