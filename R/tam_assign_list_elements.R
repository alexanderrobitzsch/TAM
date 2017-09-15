## File Name: tam_assign_list_elements.R
## File Version: 0.03
## File Last Change: 2017-05-10 10:58:26

tam_assign_list_elements <- function(x, envir)
{
	x0 <- x
	names(x) <- NULL
	L <- length(x)
    for (cc in 1:L ){
		assign( names(x0)[cc] , x[[cc]] , envir = envir ) 	
    }		
}
