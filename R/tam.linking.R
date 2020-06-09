## File Name: tam.linking.R
## File Version: 0.345

tam.linking <- function( tamobj_list, type="Hae", method="joint",
    pow_rob_hae=1, eps_rob_hae=1e-4, theta=NULL, wgt=NULL, wgt_sd=2, fix.slope=FALSE,
    elim_items=NULL, par_init=NULL, verbose=TRUE)
{
    CALL <- match.call()
    s1 <- Sys.time()
    NM <- length(tamobj_list)
    #** theta specifications
    if (is.null(theta)){
        theta <- seq(-6,6,len=101)
    }
    if (is.null(wgt)){
        wgt <- tam_normalize_vector( stats::dnorm(theta, sd=wgt_sd ) )
    }
    theta <- matrix( theta, ncol=1)
    #--- extract parameters
    parameters_list <- list()
    for (mm in 1:NM){
        parameters_list[[mm]] <- tam_linking_extract_parameters( tamobj=tamobj_list[[mm]],
                                        elim_items=elim_items[[mm]] )
    }

    #**** LINKING
    entries <- c("linking_items", "B", "A", "AXsi", "guess", "M", "SD")
    linking_list <- list()
    linking_args <- list( theta=theta, wgt=wgt, type=type, fix.slope=fix.slope,
                            pow_rob_hae=pow_rob_hae, eps_rob_hae=eps_rob_hae,
                            par_init=par_init)

    #--- subfunction chain linking
    if (method=="chain"){
        res <- tam_linking_chain( NM=NM, parameters_list=parameters_list,
                    entries=entries, verbose=verbose, linking_args=linking_args,
                    linking_list=linking_list)
    }

    #--- subfunction joint linking
    if (method=="joint"){
        res <- tam_linking_joint(NM=NM, parameters_list=parameters_list,
                    linking_args=linking_args)
    }
    # extract output values
    M_SD <- res$M_SD
    trafo_persons <- res$trafo_persons
    trafo_items <- res$trafo_items
    N_common <- res$N_common
    linking_list <- res$linking_list
    parameters_list <- res$parameters_list
    par <- res$par_optim

    #--- OUTPUT
    s2 <- Sys.time()
    time <- c(s1, s2)
    res <- list(parameters_list=parameters_list, linking_list=linking_list, M_SD=M_SD,
                    trafo_persons=trafo_persons, trafo_items=trafo_items, N_common=N_common,
                    theta=theta, wgt=wgt, NS=NM, type=type, method=method,
                    pow_rob_hae=pow_rob_hae, eps_rob_hae=eps_rob_hae, par=par,
                    CALL=CALL,time=time)
    class(res) <- "tam.linking"
    return(res)
}
