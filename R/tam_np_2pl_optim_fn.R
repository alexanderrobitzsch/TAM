## File Name: tam_np_2pl_optim_fn.R
## File Version: 0.21


tam_np_2pl_optim_fn <- function(x, nik_ii, par0, index, desmat, eps=1e-30,
    eps0=1e-7, target_fct=2)
{
    probs <- tam_np_2pl_irf_probs(x=x, par0=par0, index=index, desmat=desmat)
    log_probs <- log(probs+eps)
    dev <- -2*sum(nik_ii * log_probs)
    return(dev)
}
