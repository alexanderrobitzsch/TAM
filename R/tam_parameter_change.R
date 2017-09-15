## File Name: tam_parameter_change.R
## File Version: 0.01
## File Last Change: 2017-05-02 11:33:20

tam_parameter_change <- function( xsi, oldxsi)
{
	xsi <- as.vector(xsi)
	oldxsi <- as.vector(oldxsi)
	res <- max( abs( xsi - oldxsi ) )
	return(res)
}
