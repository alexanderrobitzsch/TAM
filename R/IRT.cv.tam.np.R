## File Name: IRT.cv.tam.np.R
## File Version: 0.18


IRT.cv.tam.np <- function(object, kfold=10, ...)
{
    #* arrange list of arguments
    args <- as.list(object$CALL)
    args$control <- object$control
    args$control$progress <- FALSE
    args$pars_init <- object$pars
    pweights <- object$pweights
    N <- length(pweights)
    args1 <- list(...)
    args <- tam_include_arguments_in_list(args=args, args1=args1 )

    #- do cross-validation
    v <- N/kfold
    ind_vec <- floor( ( 1:N )*N/(N+1) / v ) + 1
    dev_cv <- 0
    eps <- 1E-10
    cat(paste0("|", paste0(rep("*",kfold), collapse=""), "|\n|"))
    utils::flush.console()
    for (kk in 1:kfold){
        ind_kk <- which(ind_vec==kk)
        pweights_kk <- pweights
        pweights_kk[ind_kk] <- eps
        args$pweights <- pweights_kk
        res <- do.call(what=tam.np, args=args)
        ll_individual_kk <- res$ll_individual[ind_kk]
        dev_cv <- dev_cv - 2*sum(ll_individual_kk)
        tam_cat_flush_console(label="-")
    }
    tam_cat_flush_console(label="|\n")
    #--- output
    return(dev_cv)
}
