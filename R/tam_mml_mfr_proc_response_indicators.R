## File Name: tam_mml_mfr_proc_response_indicators.R
## File Version: 0.06

tam_mml_mfr_proc_response_indicators <- function(nitems, gresp, gresp.noStep)
{
    resp.ind.list <- list( 1:nitems )
    gresp.ind <- 1 - is.na( gresp )
    gresp.noStep.ind <- 1 - is.na( gresp.noStep )
    resp.ind <- gresp.noStep.ind
    nomiss <- mean( gresp.noStep.ind )==1
    #***
    miss.items <- rep(0,nitems)
    for (ii in 1:nitems){
        resp.ind.list[[ii]] <- which( gresp.noStep.ind[,ii]==1)
        miss.items[ii] <- ii * ( length(resp.ind.list[[ii]])==0 )
    }
    gresp0.noStep <- gresp.noStep
    gresp[ is.na( gresp) ] <- 0
    gresp.noStep[ is.na( gresp.noStep) ] <- 0
    #--- OUTPUT
    res <- list(resp.ind.list=resp.ind.list, gresp.ind=gresp.ind, gresp.noStep.ind=gresp.noStep.ind,
                resp.ind=resp.ind, nomiss=nomiss, miss.items=miss.items, gresp0.noStep=gresp0.noStep,
                gresp=gresp, gresp.noStep=gresp.noStep)
    return(res)
}
