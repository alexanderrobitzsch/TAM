## File Name: tamcat.R
## File Version: 9.04
## File Last Change: 2017-09-15 17:25:43

######################################################
tamcat <- function( label , time0 , active )
{
	if (active){
		z0 <- time0
		cat( label , "  " )
		z1 <- Sys.time()
		print(z1-z0)
		z0 <- z1 	
		zout <- z0
	} else {
		zout <- NULL
	}
	return(zout)
}
######################################################	
