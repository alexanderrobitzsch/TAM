## File Name: tam_exp_overflow.R
## File Version: 0.03

tam_exp_overflow <- function(x, max=1E200 )
{
    x <- ifelse( is.na(x), -max, x )
    x <- ifelse( x > max, max, x)
    y <- exp(x)
    return(y)
}
