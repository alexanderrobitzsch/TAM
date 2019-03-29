## File Name: summary.tam.jml.R
## File Version: 9.253


#***** summary for tam object
summary.tam.jml <- function( object, file=NULL, ...)
{
    #* open sink
    tam_osink(file=file)

    cat("------------------------------------------------------------\n")
    cat( tam_packageinfo("TAM"), "\n" )
    cat( tam_rsessinfo(), "\n\n")

    cat("Start of Analysis:", paste( object$time[1] ), "\n" )
    cat("End of Analysis:", paste( object$time[2] ), "\n" )
    cat("Computation time:", print( object$time[2] - object$time[1] ), "\n\n")
    cat("Joint Maximum Likelihood Estimation in TAM \n\n")
    irtmodel <- object$irtmodel
    cat("IRT Model", irtmodel )

    # print Call
    tam_print_call(object$CALL)

    cat("------------------------------------------------------------\n")
    cat( "Number of iterations", "=", object$iter, "\n\n" )
    cat( "Deviance", "=", round( object$deviance, 2 ), " | " )
    cat( "Log Likelihood", "=", round( -object$deviance/2, 2 ), "\n" )
    cat( "Number of persons", "=", object$nstud, "\n" )

    if( ! is.null( object$formulaA)  ){
        cat( "Number of generalized items", "=", object$nitems, "\n" )
        cat( "Number of items", "=", ncol(object$resp_orig), "\n" )
    } else {
        cat( "Number of items", "=", object$nitems, "\n" )
    }

    cat("\nItem Parameters xsi\n")
    obji <- object$item
    tam_round_data_frame_print(obji=obji, digits=3, from=2)

    #** close sink
    tam_csink(file=file)
}
