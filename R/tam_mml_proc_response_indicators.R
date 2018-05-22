## File Name: tam_mml_proc_response_indicators.R
## File Version: 0.05

tam_mml_proc_response_indicators <- function( resp, nitems )
{
    # define response indicator matrix for missings
    resp.ind <- 1 - is.na(resp)
    nomiss <- sum( is.na(resp) )==0
    #*** included nomiss in M step regression
    resp.ind.list <- list( 1:nitems )
    for (ii in 1:nitems){
        resp.ind.list[[ii]] <- which( resp.ind[,ii]==1)
    }
    resp[ is.na(resp) ] <- 0     # set all missings to zero
    #--- OUTPUT
    res <- list(resp=resp, resp.ind=resp.ind, resp.ind.list=resp.ind.list, nomiss=nomiss)
    return(res)
}

