## File Name: tam_find_root.R
## File Version: 0.01


################################################################
# root finding utility function
tam_find_root <- function( x1 , y1 , prob.lvl , theta )
{
    N <- length(y1)
    dfr <- cbind( x1 , y1 )
    dfr <- dfr[ order( dfr[,1] ) , ]
    x1 <- dfr[,1]
    y1 <- dfr[,2]
    y2 <- y1 - prob.lvl
    i0 <- which( y2 < 0 )
    i1 <- which( y2 > 0 )
    thetasol <- NA
    if ( ( length(i1) > 0 ) & ( length(i0) > 0 ) ){
        i0 <- max( i0 )
        i1 <- min( i1 )
        theta0 <- theta[i0]
        theta1 <- theta[i1]
        a0 <- y2[i0]
        a1 <- y2[i1]
        slo <- ( a1 - a0 ) / ( theta1 - theta0 )
        thetasol <- theta0 - a0 / slo
    }
    return(thetasol)
}
################################################################
