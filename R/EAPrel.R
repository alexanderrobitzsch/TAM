## File Name: EAPrel.R
## File Version: 9.06

#######################################################
EAPrel <- function( theta , error , w = rep(1,length(theta) ), select=NULL )
{
    #--- select cases
    if ( ! is.null(select) ){
        theta <- theta[ select ]
        error <- error[ select ]
        w <- w[ select ]
    }
    res <- WLErel_exclude_missings(theta=theta, error=error, w=w)
    theta <- res$theta
    error <- res$error
    w <- res$w
    v1 <- weighted_var( x = theta , w = w  )
    v2 <- weighted_mean( x = error^2 , w = w )
    # v1 / (v1+v2) = 1 - v2 / ( v1 + v2 )
    rel <- v1 / ( v1 + v2 )
    return(rel)
}
#######################################################

