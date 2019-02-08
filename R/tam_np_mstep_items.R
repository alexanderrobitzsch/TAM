## File Name: tam_np_mstep_items.R
## File Version: 0.263


tam_np_mstep_items <- function(I, n.ik, desmat, Msteps, spline_optim,
    probs, pars, lambda, iter, dev, n_basis, penalty_type, target_fct,
    index_target, index_basis, model="2PL")
{
    par_reg_penalty <- rep(0,I)
    n_reg <- rep(0,I)
    regularized <- rep(FALSE,I)
    for (ii in 1:I){
        nik_ii <- n.ik[ii,,]
        n_ii <- sum(nik_ii)
        nik_ii <- nik_ii / sum(nik_ii)
        par0 <- pars[ii,]
        np <- length(par0)
        #- update item slope and intercept
        index <- index_target
        args <- list(start=par0[index], objective=tam_np_2pl_optim_fn,
                            gradient=tam_np_2pl_grad_fn, nik_ii=nik_ii, index=index,
                            desmat=desmat, par0=par0, control=list(maxit=Msteps),
                            target_fct=target_fct)
        res <- do.call( what=stats::nlminb, args=args)
        par0[index] <- res$par

        #- update spline functions
        index <- index_basis
        par_old <- par0[index]
        if (spline_optim){
            args$start <- par_old
            res1 <- do.call( what=stats::nlminb, args=args)
            par_old <- res1$par
        }
        args <- list(x=par_old, nik_ii=nik_ii, par0=par0, index=index, desmat=desmat,
                        target_fct=target_fct )
        res <- do.call(what=tam_np_2pl_optim_fn_grad_hess, args=args)
        fn0 <- res$fn0
        grad <- res$grad
        hess <- res$hess
        hess_max <- res$hess_max
        res <- tam_np_group_lasso_update(par_old=par_old, grad=grad,
                        hess_max=hess_max, lambda=lambda[ii], penalty_type=penalty_type,
                        n_ii=n_ii)
        par_reg <- res$par_reg
        regularized[ii] <- res$regularized
        par_reg_penalty[ii] <- res$par_reg_penalty
        n_reg[ii] <- res$n_reg
        par0[index] <- par_reg
        pars[ii,] <- par0
    } #--- end item ii

    #- update item response functions
    probs <- tam_np_2pl_calc_probs(pars=pars, desmat=desmat)
    if (iter>=5){
        spline_optim <- FALSE
    }
    ncol_pars <- ncol(pars)
    if (model=="2PL"){
        n_est <- I*ncol_pars - sum(n_reg)
    }
    if (model=="1PL"){
        n_est <- I*(ncol_pars-1) - sum(n_reg) + 1
    }
    AIC <- dev + 2*n_est

    #--- output
    res <- list(spline_optim=spline_optim, probs=probs, pars=pars, n_reg=n_reg,
                par_reg_penalty=par_reg_penalty, n_est=n_est, AIC=AIC,
                regularized=regularized)
    return(res)
}
