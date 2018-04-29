## File Name: tam_matrix2.R
## File Version: 0.03

tam_matrix2 <- function(x, nrow=NULL, ncol=NULL)
{
    if ( is.null(ncol) ){
        ncol <- length(x)
    }
    if ( is.null(nrow) ){
        nrow <- 1
    }
    y <- matrix(x , nrow=nrow, ncol=ncol, byrow=TRUE)
    return(y)
}
