## File Name: tam_difference_quotient.R
## File Version: 0.04

tam_difference_quotient <- function( d0 , d0p , d0m , h)
{
    d1 <- ( d0p - d0 ) / h
    d2 <- ( ( d0p - d0 ) - ( d0 - d0m ) ) / h^2
    res <- list( d1 = d1 , d2 = d2 )
    return(res)
}

tam_mml_3pl_difference_quotient <- tam_difference_quotient
