## File Name: summary.tam.linking.R
## File Version: 0.07
## File Last Change: 2017-10-20 10:09:45

summary.tam.linking <- function( object , file = NULL , ...)
{

	tam_osink( file = file)
						
	cat("------------------------------------------------------------\n")	

	#- package and R session
	tam_print_package_rsession(pack="TAM")			
	
    cat( paste0("Linking of " , object$NS , " Studies") )
    tam_print_call(object$CALL)	
	
	type <- object$type
	if (type=="Hae"){ cat("Haebara Linking Method\n")}
	if (type=="SL"){ cat("Stocking Lord Linking Method\n")}

	cat("------------------------------------------------------------\n")
	cat( "Number of Linking Items\n" )	
	obji <- object$N_common
	print(obji)
	
	cat("------------------------------------------------------------\n")
	cat( "Transformation Constants for Item Parameters\n" )	
	obji <- object$trafo_items
	obji <- round(obji,3)
	print(obji)

	cat("------------------------------------------------------------\n")
	cat( "Transformation Constants for Person Parameters\n" )	
	obji <- object$trafo_persons
	obji <- round(obji,3)
	print(obji)

	cat("------------------------------------------------------------\n")
	cat( "Means and Standard Deviations of Studies \n" )	
	obji <- object$M_SD
	obji <- round(obji,3)
	print(obji)
	
	#******
	tam_csink(file=file)
	
}
#*******************************************************
