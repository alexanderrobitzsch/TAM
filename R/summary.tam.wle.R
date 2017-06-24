
summary.tam.wle <- function( object , file = NULL , digits = 3 , ...){

	CDM::osink( file=file , suffix = "__SUMMARY.Rout" )

	print.tam.wle(x=object, digits = digits , ...)

	CDM::csink(file=file)	
}
#*******************************************************
