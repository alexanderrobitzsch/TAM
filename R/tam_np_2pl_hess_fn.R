## File Name: tam_np_2pl_hess_fn.R
## File Version: 0.12


tam_np_2pl_hess_fn <- function(x, nik_ii, par0, index, desmat, target_fct=2)
{
    probs <- tam_np_2pl_irf_probs(x=x, par0=par0, index=index, desmat=desmat)
    m1 <- (nik_ii[1,]+nik_ii[2,]) * probs[1,]*probs[2,]
    hess <- 2*tam_colSums(desmat[,index]^2 * m1 )
    return(hess)
}
