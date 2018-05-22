## File Name: summary_tamaan_3pl_intro.R
## File Version: 9.18


################################################
# tamaan summary 3pl introduction
summary_tamaan_3pl_intro <- function(object){

    cat("------------------------------------------------------------\n")

    #- package and R session
    tam_print_package_rsession(pack="TAM")
    #- computation time
    tam_print_computation_time(object=object)

    cat("Multidimensional Item Response Model in TAM \n\n")
    irtmodel <- object$irtmodel
    cat("IRT Model", irtmodel, " (Function 'tam.mml.3pl')\n")

    cat("------------------------------------------------------------\n")
    cat( "Number of iterations=", object$iter, "\n\n" )

    ctr <- object$control
    cat("Skill space:", ifelse(object$skillspace=="normal",
                "Normal Distribution", "Discrete Distribution" ), "\n")
    if (object$skillspace=="normal"){
        if (ctr$snodes==0){
            cat("Numeric integration with", dim(object$theta)[1], "integration points\n")
        }
        if (ctr$snodes>0){
            if (ctr$QMC){
                cat("Quasi Monte Carlo integration with", dim(object$theta)[1], "integration points\n")
            }
            if (! ctr$QMC){
                cat("Monte Carlo integration with", dim(object$theta)[1], "integration points\n")
            }
        }
    }
    cat( "\nDeviance=", round( object$deviance, 2 ), " | " )
    cat( "Log Likelihood=", round( -object$deviance/2, 2 ), "\n" )
    cat( "Number of persons=", object$nstud, "\n" )

    if( ! is.null( object$formulaA)  ){
        cat( "Number of generalized items=", object$nitems, "\n" )
        cat( "Number of items=", ncol(object$resp_orig), "\n" )
    } else {
        cat( "Number of items=", object$nitems, "\n" )
    }

    cat( "Number of estimated parameters=", object$ic$Npars, "\n" )
    cat( "    Item threshold parameters=", object$ic$Nparsxsi, "\n" )
    cat( "    Item slope parameters    =", object$ic$NparsB, "\n" )
    cat( "      Non-active item slopes =",
                                object$ic$Ngamma.nonactive, "\n" )
    cat( "    Item guessing parameters =", object$ic$Nguess, "\n" )
    cat( "    Regression parameters    =", object$ic$Nparsbeta, "\n" )
    cat( "    (Co)Variance parameters  =", object$ic$Nparscov, "\n" )
    cat( "    Delta parameters         =", object$ic$Ndelta, "\n\n" )

    #--- print information criteria
    tam_summary_print_ic( object=object )
}
########################################################

summary.tamaan.3pl.intro <- summary_tamaan_3pl_intro
