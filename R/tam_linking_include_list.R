## File Name: tam_linking_include_list.R
## File Version: 0.021

tam_linking_include_list <- function( list1, list2 )
{
    N2 <- length(list2)
    for (nn in 1L:N2){
        list1[[ names(list2)[nn] ]] <- list2[[ nn ]]
    }
    return(list1)
}
