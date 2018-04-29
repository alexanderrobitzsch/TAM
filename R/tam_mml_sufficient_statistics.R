## File Name: tam_mml_sufficient_statistics.R
## File Version: 0.21

tam_mml_sufficient_statistics <- function( nitems , maxK, resp, resp.ind ,
        pweights , cA, progress, use_rcpp=TRUE )
{
    cA[is.na(cA)] <- 0
    col.index <- rep( 1:nitems , each = maxK )

    #-- apply function for calculating sufficient statistics
    if ( ! use_rcpp ){
        res <- tam_mml_sufficient_statistics_R( nitems=nitems, maxK=maxK, resp=resp,
                    resp.ind=resp.ind, pweights=pweights, cA=cA, col.index=col.index )
    } else {
        res <- tam_rcpp_calc_suff_stat( resp=resp, resp_ind=resp.ind, maxK=maxK,
                    nitems=nitems, pweights=pweights, cA=cA )
    }
    cResp <- res$cResp
    ItemScore <- res$ItemScore
    if (progress){
        cat("    * Calculated Sufficient Statistics   (",
        paste(Sys.time()) , ")\n")
        utils::flush.console()
    }
    #--- OUTPUT
    res <- list(cResp=cResp, ItemScore=ItemScore, col.index=col.index)
    return(res)
}
