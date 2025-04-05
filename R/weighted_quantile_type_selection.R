## File Name: weighted_quantile_type_selection.R
## File Version: 0.102



# selection of parameters for weighted data
weighted_quantile_type_selection <- function( type, pp, N, dfr, weights_NULL)
{
    # type 4
    eps <- 1E-10
    mm <- NULL
    set1 <- FALSE
    if ( ! weights_NULL ){
        type <- -9
        a1 <- dfr$w_cum <=pp
        if ( sum(a1) > 0 ){
            ind <- which( a1 )
        } else {
            ind <- 0
        }
        jj <- max(ind)
        jj1 <- jj + 1
        if (jj1 > N){ jj1 <- N}
        if (jj %in% c(0,-Inf)){
            jj <- 1
            set1 <- TRUE
            jj1 <- 1
        }
        if ( jj !=jj1){
            GAMMA0 <- ( pp - dfr[jj,'w_cum'] )/
                            ( eps + dfr[jj1,'w_cum'] - dfr[jj,'w_cum'] )
        } else {
            GAMMA0 <- 0
        }
        GAMMA <- GAMMA0
    }
    # type 6
    if (type==6){
        mm <- pp
        jj <- floor(N*pp + mm)
        gg <- N*pp + mm - jj
        GAMMA <- gg
    }
    # type 7
    if (type==7){
        mm <- 1 - pp
        jj <- floor(N*pp + mm)
        gg <- N*pp + mm - jj
        GAMMA <- gg
    }

    if ( ! set1){
        jj1 <- jj+1
    }
    if (jj1 > N){ jj1 <- N}
    if (jj==0){ jj <- 1}
    #--- output
    res <- list(mm=mm, jj=jj, GAMMA=GAMMA, jj1=jj1)
    return(res)
}
