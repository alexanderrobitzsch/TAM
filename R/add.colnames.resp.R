## File Name: add.colnames.resp.R
## File Version: 9.101

add.colnames.resp <- function(resp)
{
    if( is.null(colnames(resp)) ){
        I <- ncol(resp)
        colnames(resp) <- paste0('I',1L:I)
    }
    return(resp)
}
