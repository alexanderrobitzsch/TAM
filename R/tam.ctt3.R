## File Name: tam.ctt3.R
## File Version: 9.202

tam.ctt3 <- function( resp, wlescore=NULL, group=NULL, allocate=30, progress=TRUE,
    max_ncat=30, pweights=NULL)
{
    NV <- ncol(resp)
    elim <- c()
    for (vv in 1:NV){
        l1 <- length( unique(resp[,vv]) )
        if (l1 > max_ncat){
            elim <- c(elim, vv )
        }
    }
    if (length(elim)>0){
        resp <- resp[, -elim, drop=FALSE]
    }
    res <- tam_ctt_wrapper(resp=resp, wlescore=wlescore, group=group, allocate=allocate,
                progress=progress, version=3, wgt=pweights)
    return(res)
}

