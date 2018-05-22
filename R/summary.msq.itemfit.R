## File Name: summary.msq.itemfit.R
## File Version: 9.19

#**** summary for msq.itemfit
summary.msq.itemfit <- function( object, file=NULL,  ... )
{

    tam_osink( file=file)

    cat("------------------------------------------------------------\n")

    #- package and R session
    tam_print_package_rsession(pack="TAM")
    #- computation time
    tam_print_computation_time(object=object)

    cat("MSQ item fit statitics (Function 'msq.itemfit')")

    #--- print call
    tam_print_call(object$CALL)

    cat("****************************************************\n")
    cat("\nSummary outfit and infit statistic\n")
    obji <- object$summary_itemfit
    tam_round_data_frame_print(obji=obji, from=2, digits=3, rownames_null=TRUE)

    cat("\n****************************************************\n")
    cat("\nOutfit and infit statistic\n")
    obji <- object$itemfit
    ind <- grep( "fitgroup", colnames(obji) )
    tam_round_data_frame_print(obji=obji, from=ind+1, digits=3, rownames_null=FALSE)

    tam_csink(file=file)
}
###################################################
