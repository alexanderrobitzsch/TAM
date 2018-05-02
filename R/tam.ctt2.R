## File Name: tam.ctt2.R
## File Version: 9.14

tam.ctt2 <- function( resp , wlescore=NULL , group=NULL , allocate=30, progress=TRUE)
{
    res <- tam_ctt_wrapper(resp=resp, wlescore=wlescore, group=group, allocate=allocate, 
                progress=progress, version = 2)
    return(res)
}
