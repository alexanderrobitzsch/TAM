## File Name: tam_cov_wt.R
## File Version: 0.08


tam_cov_wt <- function(x, wt=NULL, method="ML")
{
    if ( is.vector(x) ){
        x <- matrix(x, ncol=1)
    }
    D <- ncol(x)
    if ( is.null(wt) ){
        wt <- rep( 1 / nrow(x), nrow(x) )
    }
    x <- as.data.frame(x)
    variance_gg <- stats::cov.wt( x=x, wt=wt, cor=FALSE, method=method)$cov
    variance_gg <- matrix( variance_gg, nrow=D, ncol=D )
    return(variance_gg)
}
