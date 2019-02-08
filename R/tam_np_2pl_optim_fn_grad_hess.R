## File Name: tam_np_2pl_optim_fn_grad_hess.R
## File Version: 0.11

tam_np_2pl_optim_fn_grad_hess <- function(x, nik_ii, par0, index, desmat,
    target_fct=2)
{
    args <- list(x=x, nik_ii=nik_ii, par0=par0, index=index, desmat=desmat,
                target_fct=target_fct)
    fn0 <- do.call(what=tam_np_2pl_optim_fn, args=args)
    grad <- do.call(what=tam_np_2pl_grad_fn, args=args)
    hess <- do.call(what=tam_np_2pl_hess_fn, args=args)
    hess_max <- max(abs(hess))
    #-- output
    res <- list(fn0=fn0, grad=grad, hess=hess, hess_max=hess_max)
    return(res)
}
