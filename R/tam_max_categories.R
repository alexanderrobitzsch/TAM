## File Name: tam_max_categories.R
## File Version: 0.05

tam_max_categories <- function(resp)
{
    if (is.vector(resp)){
        resp <- matrix(resp, ncol=1)
    }
    I <- ncol(resp)
    if (I>1){
        res <- apply( resp , 2 , max , na.rm=TRUE )
    } else {
        res <- max(resp[,1], na.rm=TRUE)
    }
    return(res)
}
