## File Name: tam_ginv_scaled.R
## File Version: 0.07

tam_ginv_scaled <- function(x, use_MASS=TRUE)
{
    x_diag <- sqrt( diag(x) )
    X1 <- tam_outer(x_diag, x_diag)
    x <- x / X1
    if (use_MASS){
        xinv <- tam_import_MASS_ginv( X=x )
    } else {
        xinv <- tam_ginv(x=x)
    }
    x_diag1 <- 1 / x_diag
    X1 <- tam_outer( x_diag1, x_diag1 )
    xinv <- xinv * X1
    return(xinv)
}
