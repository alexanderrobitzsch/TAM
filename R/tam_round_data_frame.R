## File Name: tam_round_data_frame.R
## File Version: 0.01
## File Last Change: 2017-09-15 09:52:15

tam_round_data_frame <- function(obji, from=1, to=ncol(obji), digits=3)
{
	if ( is.matrix(obji) ){
		for ( vv in from:to ){
			obji[,vv] <- round( obji[,vv] , digits )
		}
	}
	if ( is.vector(obji) ){
		obji <- round(obji, digits)
	}
	return(obji)
}
