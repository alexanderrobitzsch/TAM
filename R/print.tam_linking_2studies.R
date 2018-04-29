## File Name: print.tam_linking_2studies.R
## File Version: 0.05

print.tam_linking_2studies <- function( x, ...)
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
