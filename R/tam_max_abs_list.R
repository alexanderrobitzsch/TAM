## File Name: tam_max_abs_list.R
## File Version: 0.01



tam_max_abs_list <- function( list1, list2 )
{
    list_names <- names(list1)
    v1 <- 0
    for (name in list_names){
        v2 <- tam_max_abs( list1, list2, label=name)
        v1 <- max(v1, v2, na.rm=TRUE)
    }
    return(v1)
}
