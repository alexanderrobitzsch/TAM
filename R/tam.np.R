## File Name: tam.np.R
## File Version: 0.25


tam.np <- function( dat, probs_init=NULL, pweights=NULL, control=list() )
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
    con <- res$con
    con1a <- res$con1a

    #--- initial values
    res <- tam_np_inits( dat=dat, nodes=nodes, pweights=pweights, probs_init=probs_init )
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

    while( iterate ){
        #-- progress
        res <- tam_mml_progress_em0(progress=progress, iter=iter, disp=disp)
        #-- previous values
        probs0 <- probs
        dev0 <- dev
        #-- posterior
        res <- tam_rcpp_tam_np_posterior( dat2=dat2, dat_resp=dat_resp,
                    probs0=as.vector(probs0), pi_k=pi.k, pweights=pweights, K1=K1 )
        f.yi.qk <- res$fyiqk
        f.qk.yi <- res$fqkyi
        n.ik <- array(res$nik, dim=prob_dim )
        N.ik <- array(res$Nik, dim=c(I, TP) )
        probs <- array(res$probs, dim=prob_dim )
        ll <- res$ll
        dev <- -2*ll
        #-- parameter change
        probs_change <- max( sqrt( (probs - probs0)^2 *pi_k_array ) )
        devch <- - ( dev - dev0 )
        deviance_change <- devch
        rel_deviance_change <- deviance_change / dev0
        # progress
        res <- tam_mml_progress_em(progress=progress, deviance=dev,
                    deviance_change=deviance_change, iter=iter,
                    rel_deviance_change=rel_deviance_change, is_mml_3pl=FALSE, xsi_change=0,
                    beta_change=0, variance_change=0, B_change=0, skillspace='np', delta_change=0,
                    digits_pars=6, devch=devch,    penalty_xsi=0, is_np=TRUE, is_latreg=TRUE,
                    np_change=probs_change )
        #-- convergence
        iter <- iter + 1
        if (iter > maxiter){
            converged <- FALSE
            iterate <- FALSE
        }
        if ( ( probs_change < conv) & ( abs(devch) < convD ) ){
            iterate <- FALSE
        }

    }
    #**** end EM algorithm
    n.ik <- array( n.ik, dim=c(prob_dim, 1) )

    #--- output
    s2 <- Sys.time()
    time <- c(s1, s2, s2-s1)
    res <- list( dat=dat, dat2=dat2, dat_resp=dat_resp, n.ik=n.ik, N.ik=N.ik,
                rprobs=probs, pi.k=pi.k, pweights=pweights, like=f.yi.qk,
                hwt=f.qk.yi, iter=iter, converged=converged, time=time,
                dev=dev, theta=theta, G=1, pid=NULL)
    class(res) <- "tam.np"
    return(res)
}
