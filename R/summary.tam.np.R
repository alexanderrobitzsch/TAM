## File Name: summary.tam.np.R
## File Version: 0.20


summary.tam.np <- function( object, file=NULL, ...)
{
    tam_osink(file=file)

    sdisplay <- tam_summary_display()
    cat(sdisplay)

    #--- package and R session
    tam_print_package_rsession(pack="TAM")
    #--- computation time
    tam_print_computation_time(object=object)

    cat("Unidimensional Non- or Semiparametric Response Model in TAM \n\n")

    #--- print call
    tam_print_call(object$CALL)

    cat(sdisplay)
    cat( "Number of iterations", "=", object$iter, "\n" )
    cat("Numeric integration with", dim(object$theta)[1], "integration points\n")

    digits_ll <- 2    # digits after decimal for log-likelihood
    digits_pen <- 6
    cat( "\nDeviance", "=", round( object$dev, digits_ll ), "\n" )
    cat( "Log likelihood", "=", round( object$ic$loglike, digits_ll ), "\n" )
    cat( "Penalty value", "=", round( object$pen_val, digits_pen ), "\n" )

    cat( "Number of persons", "=", object$ic$n, "\n" )
    cat( "Number of estimated parameters", "=", object$ic$np, "\n" )
    cat( "Number of regularized parameters", "=", sum(object$n_reg), "\n\n" )

    #--- print information criteria
    res <- tam_summary_print_ic( object=object, digits_values=digits_ll )

    cat(sdisplay)
    cat("Trait distribution\n")
    cat( paste0("M=0 | ","SD=", round(object$sigma,3), "\n\n"))

    if (object$use_basis){
        cat(sdisplay)
        cat("Estimated item parameters\n\n")
        cat("Target model:", object$model, "\n")
        cat("Basis function type:", object$basis_type, "\n")
        cat("Orthonormal basis:", object$orthonormalize, "\n")
        cat("Penalty type:", object$penalty_type, "\n")
        cat("\n")
        obji <- object$item
        tam_round_data_frame_print(obji=obji, from=3, digits=3, rownames_null=TRUE)
    }

    #** close sink
    tam_csink(file=file)
}
