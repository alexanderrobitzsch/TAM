## File Name: weighted_quantile.R
## File Version: 9.09

#####################################################
# weighted_quantile
weighted_quantile <- function( x, w=rep(1,length(x)), probs=seq(0,1,.25),
            type=NULL, select=NULL)
{
    res <- tam_weighted_stats_select(x=x, w=w, select=select)
    x <- res$x
    w <- res$w
    dfr <- data.frame( x, w )
    dfr <- dfr[ ! is.na(x), ]
    dfr <- dfr[ order(dfr$x), ]
    N <- nrow(dfr)
    weights_NULL <- if( stats::sd(w)==0 ){ TRUE } else { FALSE }

    #*** reweighting
    if ( weights_NULL){
        dfr$w <- dfr$w * N / sum(dfr$w)
    }
    if ( ! weights_NULL){
        if ( is.null(type) ){ type <- "i/n" }
    }

    #*** init vector of quantiles
    PP <- length(probs)
    res <- rep(NA,PP)
    names(res) <- paste0( 100*probs, "%")
    dfr$w_cum <- cumsum( dfr$w )
    dfr$w_cum <- dfr$w_cum / max( dfr$w_cum)
    for (kk in 1:PP){
        pp <- probs[kk]
        #*** specifications according to type
        res1 <- weighted_quantile_type_selection( type=type, pp=pp, N=N,
                    dfr=dfr, weights_NULL=weights_NULL )
        jj <- res1$jj
        GAMMA <- res1$GAMMA
        jj1 <- res1$jj1
        quant_pp <- (1-GAMMA)*dfr[jj,1] + GAMMA * dfr[jj1,1]
        res[kk] <- quant_pp
    }
    return(res)
}


