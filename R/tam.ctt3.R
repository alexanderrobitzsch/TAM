## File Name: tam.ctt3.R
## File Version: 9.12

tam.ctt3 <- function( resp , wlescore=NULL , group=NULL , allocate=30, progress=TRUE)
{
    res <- tam_ctt_wrapper(resp=resp, wlescore=wlescore, group=group, allocate=allocate,
                progress=progress, version = 3)
    return(res)
}

