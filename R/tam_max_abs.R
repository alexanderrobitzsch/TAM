## File Name: tam_max_abs.R
## File Version: 0.01

tam_max_abs <- function( list1, list2, label )
{
    res <- max( abs( list1[[ label ]] - list2[[ label ]]), na.rm=TRUE )
    return(res)
}
