## File Name: tam_mml_mfr_proc_delete_missing_items.R
## File Version: 0.04

tam_mml_mfr_proc_delete_missing_items <- function(miss.items, delete.red.items, maxK,
    gresp, gresp.noStep, gresp.noStep.ind, A, B, resp.ind.list, resp.ind, nitems,
    pweightsM, pweights, nstud, progress)
{
    miss.items <- miss.items[ miss.items > 0 ]
    if ( length(miss.items) == 0 ){
        delete.red.items <- FALSE
    }
    miss.itemsK <- NULL
    if (delete.red.items){
        for (kk in 1:maxK ){
            miss.itemsK <- c( miss.itemsK , ( miss.items - 1 )* maxK + kk )
        }
        miss.itemsK <- sort(miss.itemsK)
        gresp <- gresp[ , - miss.itemsK ]
        gresp.noStep <- gresp.noStep[ , - miss.items ]
        gresp.noStep.ind <- gresp.noStep.ind[ , - miss.items ]
        A <- A[ - miss.items , , , drop=FALSE]
        B <- B[ - miss.items , , ,drop=FALSE]
        resp.ind.list <- resp.ind.list[ - miss.items ]
        resp.ind <- resp.ind[ , - miss.items ]
        nitems <- ncol(gresp.noStep)
        pweightsM <- outer( pweights , rep(1,nitems) )
        if (progress){
            cat("    * Reduced Response Data:" , nstud , "Persons and " ,
                ncol(gresp.noStep) , "Generalized Items (" , paste(Sys.time()) ,")\n" )  ;
            utils::flush.console()
        }
    }
    #--- OUPUT
    res <- list(miss.itemsK =miss.itemsK, miss.items=miss.items, delete.red.items=delete.red.items,
                A=A, B=B, gresp=gresp, gresp.noStep=gresp.noStep, gresp.noStep.ind=gresp.noStep.ind,
                resp.ind.list=resp.ind.list, resp.ind=resp.ind, nitems=nitems, pweightsM=pweightsM )
    return(res)
}
