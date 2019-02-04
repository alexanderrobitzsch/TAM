## File Name: tam_np_2pl_calc_probs.R
## File Version: 0.02

tam_np_2pl_calc_probs <- function(pars, desmat)
{
    I <- nrow(pars)
    np <- ncol(pars)
    TP <- nrow(desmat)
    probs <- array(0, dim=c(I,2,TP))
    for (ii in 1L:I){
        par0 <- pars[ii,]
        probs[ii,,] <- tam_np_2pl_irf_probs(x=par0, par0=par0, index=1:np, desmat=desmat)
    }
    return(probs)
}
