## File Name: tam.ctt3.R
## File Version: 9.17

tam.ctt3 <- function( resp, wlescore=NULL, group=NULL, allocate=30, progress=TRUE,
    max_ncat=30)
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
        resp <- resp[, -elim]
    }
    res <- tam_ctt_wrapper(resp=resp, wlescore=wlescore, group=group, allocate=allocate,
                progress=progress, version=3)
    return(res)
}

