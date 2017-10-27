## File Name: summary.tam.wle.R
## File Version: 0.04

summary.tam.wle <- function( object , file = NULL , digits = 3 , ...)
{
	tam_osink(file=file)
	
	print.tam.wle(x=object, digits = digits , ...)
	
	tam_csink(file=file)	
}
