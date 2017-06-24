

summary.tam_linking_2studies <- function( object , file = NULL , ...)
{

	CDM::osink( file = file , suffix = "__SUMMARY.Rout" )
						
	cat("------------------------------------------------------------\n")	
	cat( tam_packageinfo("TAM") , "\n" )	
	cat( tam_rsessinfo() , "\n\n")				
	
    cat("Linking of Two Studies")
    tam_print_call(object$CALL)	
	
	type <- object$type
	if (type=="Hae"){ cat("Haebara Linking Method\n")}
	if (type=="SL"){ cat("Stocking Lord Linking Method\n")}
	
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
	CDM::csink(file)
	
}
#*******************************************************
