## File Name: tam_cat_flush_console.R
## File Version: 0.01

tam_cat_flush_console <- function(label)
{
    cat(label)
    utils::flush.console()
}
