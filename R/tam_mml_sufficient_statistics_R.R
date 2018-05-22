## File Name: tam_mml_sufficient_statistics_R.R
## File Version: 0.07

tam_mml_sufficient_statistics_R <- function( nitems, maxK, resp, resp.ind,
        pweights, cA, col.index)
{
    cResp <- (resp +1) *resp.ind
    cResp <- cResp[, col.index  ]
    cResp <- 1 * ( cResp==matrix( rep(1:(maxK), nitems), nrow(cResp),
                    ncol(cResp), byrow=TRUE ) )
    if ( stats::sd(pweights) > 0 ){
        ItemScore <- as.vector( t( colSums( cResp * pweights ) ) %*% cA )
    } else {
        ItemScore <- as.vector( t( colSums( cResp) ) %*% cA )
    }
    #--- output
    res <- list(cResp=cResp, ItemScore=ItemScore)
    return(res)
}
