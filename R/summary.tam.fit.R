## File Name: summary.tam.fit.R
## File Version: 9.17


#*** summary for tam.fit
summary.tam.fit <- function( object, file=NULL, ... )
{
    tam_osink(file=file)

    sdisplay <- tam_summary_display()
    cat(sdisplay)

    #- package and R session
    tam_print_package_rsession(pack="TAM")
    #- computation time
    tam_print_computation_time(object=object)

    cat("Item fit statitics (Function 'tam.fit')")

    #--- print call
    tam_print_call(object$CALL)

    obji <- object$itemfit
    ind <- grep( "pholm", colnames(obji) )
    obji <- obji[, - ind ]
    tam_round_data_frame_print(obji=obji, digits=3, from=2)

    tam_csink(file=file)
}

