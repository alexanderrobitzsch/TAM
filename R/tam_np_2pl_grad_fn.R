## File Name: tam_np_2pl_grad_fn.R
## File Version: 0.15

tam_np_2pl_grad_fn <- function(x, nik_ii, par0, index, desmat, eps0=1e-7,
    penalty_type="lasso", lambda=0, target_fct=2)
{
    probs <- tam_np_2pl_irf_probs(x=x, par0=par0, index=index, desmat=desmat)
    m1 <- nik_ii[1,] * probs[2,] - nik_ii[2,] * probs[1,]
    grad <- 2*tam_colSums(desmat[,index] * m1 )
    pen <- tam_np_optim_ridge_penalty(par0=par0, penalty_type=penalty_type,
                target_fct=target_fct, lambda=lambda, index=index, deriv=1)
    grad <- grad + pen
    return(grad)
}
