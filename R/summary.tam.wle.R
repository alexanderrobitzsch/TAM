## File Name: summary.tam.wle.R
## File Version: 0.04
## File Last Change: 2017-09-16 13:33:54

summary.tam.wle <- function( object , file = NULL , digits = 3 , ...)
{
	tam_osink(file=file)
	
	print.tam.wle(x=object, digits = digits , ...)
	
	tam_csink(file=file)	
}
