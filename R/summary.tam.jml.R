## File Name: summary.tam.jml.R
## File Version: 9.258


#***** summary for tam object
summary.tam.jml <- function( object, file=NULL, ...)
{
    #* open sink
    tam_osink(file=file)
    sdisplay <- tam_summary_display()

    cat(sdisplay)
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

    cat(sdisplay)
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
    cat( "constraint", "=", object$constraint, "\n" )
    cat( "bias", "=", object$bias, "\n" )

    obji <- object$theta_summary
    if (obji$ndim==1){
        cat(sdisplay)
        cat("Person Parameters xsi\n")
        cat( "M", "=", round( obji$M, 2 ), "\n" )
        cat( "SD", "=", round( obji$SD, 2 ), "\n" )
    }

    cat(sdisplay)
    cat("Item Parameters xsi\n")
    obji <- object$item
    tam_round_data_frame_print(obji=obji, digits=3, from=2)

    cat(sdisplay)
    cat("Item Parameters -A*Xsi\n")
    obji <- object$item1
    tam_round_data_frame_print(obji=obji, from=2, to=ncol(obji), digits=3, rownames_null=TRUE)

    #** close sink
    tam_csink(file=file)
}
