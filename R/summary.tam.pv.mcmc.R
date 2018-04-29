## File Name: summary.tam.pv.mcmc.R
## File Version: 0.21

#*******************************************************
# summary
summary.tam.pv.mcmc <- function( object , file = NULL , ...)
{

    tam_osink( file = file )

    calc_ic <- object$calc_ic

    cat("------------------------------------------------------------\n")

    #- package and R session
    tam_print_package_rsession(pack="TAM")
    #- computation time
    tam_print_computation_time(object=object)

    cat("Bayesian Estimation of Plausible Values")

    #- print call
    tam_print_call(object$CALL)

    cat("------------------------------------------------------------\n")
    cat( "Number of iterations =" , object$n.iter , "\n" )
    cat( "Number of burnin iterations =" , object$n.burnin , "\n" )

    cat("------------------------------------------------------------\n")

    if (calc_ic){
        cat("\nCriteria based on Marginal Likelihood\n")
        cat( "\nDeviance = " , round( object$ic$deviance , 2 ) , " | " )
        cat( "Log Likelihood = " , round( -object$ic$deviance/2 , 2 ) , "\n" )
    }
    cat( "Number of persons = " , object$ic$n , "\n" )
    cat( "Number of estimated parameters = " , object$ic$Npars , "\n\n" )

    #--- print information criteria
    tam_summary_print_ic( object=object, bayes_crit=TRUE )

    #----- properties of plausible values
    cat("------------------------------------------------------------\n")
    cat("Plausible Values\n\n")

    cat( "Number of plausible values =" , attr(object$pv, "nplausible") , "\n" )
    cat( "Iterations between PVs =" , attr(object$pv, "pv_lag") , "\n" )
    cat( "Average acceptance rate =" ,
                round( attr(object$theta_acceptance_MH, "M_accrate"),3) , "\n" )
    cat( "Average MH adjustment factor =" ,
                round( attr(object$theta_acceptance_MH, "M_adj_MH"),3) , "\n" )

    cat("\nAutocorrelation of Drawn Plausible Values\n")
    tam_round_data_frame_print(obji=object$theta_acf, digits=3)

    cat("\nEAP Reliability\n")
    tam_round_data_frame_print(obji=object$EAP_rel, digits=3)

    cat("------------------------------------------------------------\n")

    cat("Regression Parameters\n")
    obji <- object$parameter_summary
    tam_round_data_frame_print(obji=obji, from=2, digits=3)

    #*** print covariance matrix
    cov_digits <- 3
    tam_pv_summary_covariance( obji=object$variance,
                label="Residual Covariance Matrix", digits=cov_digits)

    #*** print correlation matrix
    tam_pv_summary_covariance( obji=object$correlation,
                label="Residual Correlation Matrix", digits=cov_digits)

    #******
    tam_csink(file=file)
}
#*******************************************************
