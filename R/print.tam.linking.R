## File Name: print.tam.linking.R
## File Version: 0.01
## File Last Change: 2017-06-23 12:22:14

print.tam.linking <- function(x, ...)
{
	object <- x
	type <- object$type
	if (type=="Hae"){ cat("Haebara Linking Method")}
	if (type=="SL"){ cat("Stocking Lord Linking Method")}
    tam_print_call(object$CALL)		
	cat( "\nTransformation Constants for Item Parameters\n" )	
	obji <- object$trafo_items
	obji <- round(obji,3)
	print(obji)
}
