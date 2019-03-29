## File Name: tam_include_arguments_in_list.R
## File Version: 0.03

tam_include_arguments_in_list <- function(args, args1)
{
    NL <- length(args1)
    names1 <- names(args1)
    if (NL>0){
        for (ll in 1L:NL){
            args[[ names1[ll] ]] <- args1[[ names1[ll] ]]
        }
    }
    return(args)
}
