## File Name: tam_weighted_stats_select.R
## File Version: 0.03

tam_weighted_stats_select <- function(x, w, select)
{
    if ( ! is.null(select) ){
        x <- x[ select ]
        w <- w[ select ]
    }
    #-- remove missing cases
    ind <- which( ! is.na(x) )
    x <- x[ind]
    w <- w[ind]
    #-- output
    res <- list( x = x, w=w)
    return(res)
}
