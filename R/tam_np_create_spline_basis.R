## File Name: tam_np_create_spline_basis.R
## File Version: 0.409

tam_np_create_spline_basis <- function(nodes, n_basis, items, pi.k, dat,
    target_fct=2, basis_type, lambda=NULL, pars_init=NULL, orthonormalize=TRUE,
    model="2PL")
{
    est_bspline <- FALSE
    desm <- NULL
    pars <- NULL
    Wt <- NULL
    Wt_inv <- NULL
    I <- length(items)
    NB <- 0
    est_bspline <- TRUE

    #* B-spline basis
    if (basis_type=="bspline"){
        desm <- tam_np_create_spline_basis_bsplines(nodes=nodes, n_basis=n_basis)
    }
    #* Hermite polynomial
    if (basis_type=="hermite"){
        orthonormalize <- TRUE
        if (n_basis<=1){
            orthonormalize <- FALSE
        }
        desm <- tam_np_create_spline_basis_hermite(nodes=nodes, n_basis=n_basis)
    }
    #* extend design matrix for 1PL model
    if (model=="1PL"){
        nc <- ncol(desm)
        desm <- desm[, c(1,2,2:nc)]
    }
    desm_orig <- desm <- desm[, colMeans( abs(desm) ) > 0 ]
    ND <- nrow(desm)

    #- orthonormalize design matrix
    if (orthonormalize){
        x <- desm[, - seq_len(target_fct) ]
        w <- pi.k
        res <- tam_orthonormalize_design_matrix(x=x, w=pi.k, eps=1e-8)
        xo <- res$xo
        Wt <- res$Wt
        Wt_inv <- res$Wt_inv
        desm <- cbind( desm[, seq_len(target_fct) ], xo )
    }
    NB <- ncol(desm) - target_fct
    colnames(desm) <- c("d","a", paste0("s",1:NB) )
    d_init <- stats::qlogis( colMeans(dat, na.rm=TRUE) )
    pars <- matrix(0, nrow=I, ncol=NB+2)
    colnames(pars) <- colnames(desm)
    rownames(pars) <- items
    pars[,"a"] <- 1
    pars[,"d"] <- d_init
    if (is.null(lambda)){
        lambda <- 0
    }
    if ( length(lambda)==1 ){
        lambda <- rep(lambda, I)
    }
    if (!is.null(pars_init)){
        pars <- pars_init
    }
    #- compute item response functions
    probs <- tam_np_2pl_calc_probs(pars=pars, desmat=desm)
    spline_optim <- FALSE

    if (model=="2PL"){
        index_target <- 1:2
        index_basis <- 2 + 1:n_basis
    }
    if (model=="1PL"){
        index_target <- 1
        index_basis <- 2 + 1:(n_basis+1)
    }

    # maximum number of regularized parameters
    nb <- n_basis
    if (model=="1PL"){ nb <- nb + 1 }
    n_reg_max <- I*nb

    #-- output
    res <- list(est_bspline=est_bspline, desmat=desm, pars=pars, NB=NB,
                Wt=Wt, Wt_inv=Wt_inv, desmat_orig=desm_orig, lambda=lambda,
                probs=probs, orthonormalize=orthonormalize, spline_optim=spline_optim,
                target_fct=target_fct, index_target=index_target,
                index_basis=index_basis, n_reg_max=n_reg_max)
    return(res)
}
