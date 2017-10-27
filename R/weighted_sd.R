## File Name: weighted_sd.R
## File Version: 0.02


#######################################################################	
# standard deviation
weighted_sd <- function( x , w=rep(1,length(x) ) , method = "unbiased" ,
		select=NULL)
{
	res <- sqrt( weighted_var(x=x, w=w, method=method, select=select) )
	return(res)
}
#######################################################################
