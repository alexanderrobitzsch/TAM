## File Name: tam.np.R
## File Version: 0.421


tam.np <- function( dat, probs_init=NULL, pweights=NULL, lambda=NULL, control=list(),
    model="2PL", n_basis=0, basis_type="hermite", penalty_type="lasso",
    pars_init=NULL, orthonormalize=TRUE)
{
    s1 <- Sys.time()
    CALL <- match.call()
    #--- attach control arguments
    disp <- "....................................................\n"
    increment.factor <- progress <- nodes <- snodes <- ridge <- xsi.start0 <- QMC <- NULL
    maxiter <- conv <- convD <- min.variance <- max.increment <- Msteps <- convM <- NULL
    R <- trim_increment <- NULL
    fac.oldxsi <- acceleration <- NULL
    e1 <- environment()
    tam_fct <- "tam.np"
    res <- tam_mml_control_list_define(control=control, envir=e1, tam_fct=tam_fct,
                prior_list_xsi=NULL)
    control <- con <- res$con
    con1a <- res$con1a

    #--- initial values
    res <- tam_np_inits( dat=dat, nodes=nodes, pweights=pweights,
                probs_init=probs_init, n_basis=n_basis, model=model )
    maxK <- res$maxK
    K <- res$K
    K1 <- res$K1
    TP <- res$TP
    I <- res$I
    probs <- res$probs
    pi.k <- res$pi.k
    dat_resp <- res$dat_resp
    dat2 <- res$dat2
    N <- res$N
    pweights <- res$pweights
    theta <- res$theta
    items  <- res$items
    use_basis <- res$use_basis

    prob_dim <- c(I, K1, TP)
    dev <- 1E200
    iterate <- TRUE
    converged <- TRUE
    iter <- 1
    pi_k_array <- array(NA, dim=c(I,K1,TP))
    for (ii in 1:I){
        for (kk in 1:K1){
            pi_k_array[ii,kk,] <- pi.k
        }
    }

    #- define B-spline basis
    if (use_basis){
        res <- tam_np_create_spline_basis(nodes=nodes, n_basis=n_basis, items=items,
                pi.k=pi.k, dat=dat, lambda=lambda, basis_type=basis_type,
                pars_init=pars_init, orthonormalize=orthonormalize, model=model)
        probs <- res$probs
    }
    est_bspline <- res$est_bspline
    desmat <- res$desmat
    pars <- res$pars
    lambda <- res$lambda
    orthonormalize <- res$orthonormalize
    spline_optim <- res$spline_optim
    target_fct <- res$target_fct
    index_target <- res$index_target
    index_basis <- res$index_basis
    n_reg_max <- res$n_reg_max

    #--- algorithm
    while( iterate ){
        #-- progress
        res <- tam_mml_progress_em0(progress=progress, iter=iter, disp=disp,
                    print_estep=FALSE)
        #-- previous values
        probs0 <- as.vector(probs)
        dev0 <- dev

        #-- compute posterior and nonparametric update of item response functions
        res <- tam_rcpp_tam_np_posterior( dat2=dat2, dat_resp=dat_resp,
                    probs0=probs0, pi_k=pi.k, pweights=pweights, K1=K1 )
        f.yi.qk <- res$fyiqk
        f.qk.yi <- res$fqkyi
        n.ik <- array(res$nik, dim=prob_dim )
        N.ik <- array(res$Nik, dim=c(I,TP) )
        probs <- array(res$probs, dim=prob_dim )
        ll <- res$ll
        ll_individual <- res$ll_individual
        dev <- -2*ll

        #- M-step trait distribution
        res <- tam_np_mstep_trait_distribution(nodes=nodes, f.qk.yi=f.qk.yi,
                    model=model, pi.k=pi.k)
        sigma <- res$sigma
        pi.k <- res$pi.k

        #- M-step for parametric item response functions
        if (use_basis){
            res <- tam_np_mstep_items( I=I, n.ik=n.ik, desmat=desmat,
                    Msteps=Msteps, spline_optim=spline_optim, probs=probs, pars=pars,
                    lambda=lambda, iter=iter, dev=dev, n_basis=n_basis,
                    penalty_type=penalty_type, target_fct=target_fct,
                    index_target=index_target, index_basis=index_basis, model=model)
            probs <- res$probs
        }
        spline_optim <- res$spline_optim
        pars <- res$pars
        n_reg <- res$n_reg
        par_reg_penalty <- res$par_reg_penalty
        n_est <- res$n_est
        AIC <- res$AIC
        regularized <- res$regularized

        #-- parameter change
        probs_change <- max( sqrt( (probs - probs0)^2 *pi_k_array ) )
        np_change <- probs_change
        devch <- - ( dev - dev0 )
        deviance_change <- devch
        rel_deviance_change <- deviance_change / dev0

        #-- print progress
        res <- tam_mml_progress_em(progress=progress, deviance=dev,
                    deviance_change=deviance_change, iter=iter,
                    rel_deviance_change=rel_deviance_change, is_mml_3pl=FALSE,
                    xsi_change=0, beta_change=0, variance_change=0, B_change=0,
                    skillspace='np', delta_change=0, digits_pars=6, devch=devch,
                    penalty_xsi=0, is_np=TRUE, is_latreg=TRUE, np_change=np_change,
                    par_reg_penalty=par_reg_penalty, n_reg=n_reg, AIC=AIC, 
                    n_est=n_est, n_reg_max=n_reg_max)
        #-- convergence
        iter <- iter + 1
        if (iter > maxiter){
            converged <- FALSE
            iterate <- FALSE
        }
        if ( ( probs_change < conv) & ( abs(rel_deviance_change) < convD ) ){
            iterate <- FALSE
        }

    }
    #**** end EM algorithm

    n.ik <- array( n.ik, dim=c(prob_dim, 1) )
    pi.k <- matrix(pi.k, ncol=1)
    theta <- matrix(theta, ncol=1)
    loglike <- -dev/2

    #- information criteria
    if (! use_basis){ n_est <- NA }
    ic <- tam_np_information_criteria(dev=dev, n_est=n_est, n=N)
    pen_val <- sum(par_reg_penalty)

    #- item parameters
    if (use_basis){
        ncol_pars <- ncol(pars)
        if (model=="2PL"){
            np <- ncol_pars - n_reg
        }
        if (model=="1PL"){
            np <- ncol_pars - 1 - n_reg
        }
        item <- data.frame(item=items, regul=regularized, lambda=lambda, np=np, pars)
    } else {
        item <- NULL
    }

    #--- output
    s2 <- Sys.time()
    time <- c(s1, s2)
    res <- list( CALL=CALL, dat=dat, dat2=dat2, dat_resp=dat_resp, n.ik=n.ik, 
                N.ik=N.ik, item=item, rprobs=probs, pi.k=pi.k, nodes=nodes, 
                pweights=pweights, like=f.yi.qk, hwt=f.qk.yi, iter=iter, 
                loglike=loglike, AIC=AIC, converged=converged,
                iter=iter, time=time, dev=dev, theta=theta, G=1, pars=pars, 
                n_est=n_est, n_reg=n_reg, regularized=regularized, 
                basis_type=basis_type, n_basis=n_basis, desmat=desmat, ic=ic, 
                pid=NULL, orthonormalize=orthonormalize, penalty_type=penalty_type,
                pen_val=pen_val, use_basis=use_basis, model=model, sigma=sigma,
                ll_individual=ll_individual, control=control)
    class(res) <- "tam.np"
    return(res)
}
