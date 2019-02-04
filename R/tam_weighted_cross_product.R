## File Name: tam_weighted_cross_product.R
## File Version: 0.02

tam_weighted_cross_product <- function(x, w)
{
    cov_x <- crossprod( x * w, x )
    return(cov_x)
}
