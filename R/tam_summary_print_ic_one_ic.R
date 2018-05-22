## File Name: tam_summary_print_ic_one_ic.R
## File Version: 0.13

tam_summary_print_ic_one_ic <- function(ic, crit, digits_ic=0, digits_penalty=2)
{
    ic_val <- ic[[ crit ]]
    deviance <- ic$deviance
    penalty <- ic_val - deviance
    nc <- nchar(crit)
    ic_label <- paste0( crit, rep( "", 4 - nc), collapse="")
    crit_desc <- tam_summary_print_ic_description(crit=crit)
    cat( ic_label, "=", round( ic_val, digits_ic ),
            " | penalty=", round( penalty, digits_penalty ),
            "   |", crit_desc, "\n" )
}
