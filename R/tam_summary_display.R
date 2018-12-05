## File Name: tam_summary_display.R
## File Version: 0.02

tam_summary_display <- function(symbol="-", len=60)
{
    res <- paste0( paste0( rep(symbol,len), collapse="" ), "\n")
    return(res)
}
