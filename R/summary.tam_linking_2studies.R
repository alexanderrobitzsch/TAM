## File Name: summary.tam_linking_2studies.R
## File Version: 0.09

summary.tam_linking_2studies <- function( object , file = NULL , ...)
{

    tam_osink( file = file )

    cat("------------------------------------------------------------\n")

    #- package and R session
    tam_print_package_rsession(pack="TAM")

    cat("Linking of Two Studies")
    tam_print_call(object$CALL)

    type <- object$type
    if (type=="Hae"){ cat("Haebara Linking Method\n")}
    if (type=="SL"){ cat("Stocking Lord Linking Method\n")}

    cat("------------------------------------------------------------\n")
    cat( "Transformation Constants for Item Parameters\n" )
    obji <- object$trafo_items
    tam_round_data_frame_print(obji=obji, digits=3)

    cat("------------------------------------------------------------\n")
    cat( "Transformation Constants for Person Parameters\n" )
    obji <- object$trafo_persons
    tam_round_data_frame_print(obji=obji, digits=3)

    cat("------------------------------------------------------------\n")
    cat( "Means and Standard Deviations of Studies \n" )
    obji <- object$M_SD
    tam_round_data_frame_print(obji=obji, digits=3)

    #******
    tam_csink(file=file)
}
#*******************************************************
