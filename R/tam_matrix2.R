## File Name: tam_matrix2.R
## File Version: 0.02
## File Last Change: 2017-04-29 13:24:51

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
