## File Name: weighted_var.R
## File Version: 9.131


# weighted variance
# This function is a wrapper to cov.wt
weighted_var <- function( x, w=rep(1,length(x) ), method="unbiased",
    select=NULL )
{
    res <- tam_weighted_stats_select(x=x, w=w, select=select)
    x <- res$x
    w <- res$w
    dat <- data.frame(x=x)
    res <- stats::cov.wt( x=dat, wt=w, cor=FALSE, center=TRUE, method=method )
    res <- res$cov[1,1]
    return(res)
}

