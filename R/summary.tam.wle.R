## File Name: summary.tam.wle.R
## File Version: 0.02
## File Last Change: 2017-06-02 10:08:14

summary.tam.wle <- function( object , file = NULL , digits = 3 , ...){

	CDM::osink( file=file , suffix = "__SUMMARY.Rout" )

	print.tam.wle(x=object, digits = digits , ...)

	CDM::csink(file=file)	
}
#*******************************************************
