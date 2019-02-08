## File Name: weighted_kurtosis.R
## File Version: 0.09

# kurtosis
weighted_kurtosis <- function( x, w=rep(1,length(x)), select=NULL )
{
    res <- tam_weighted_stats_select(x=x, w=w, select=select)
    x <- res$x
    w <- res$w
    m <- weighted_mean( x=x, w=w)
    v <- weighted_var( x=x, w=w)
    y <- (x-m)^4
    res <- weighted_mean(x=y, w=w) / v^2 - 3
    return(res)
}
