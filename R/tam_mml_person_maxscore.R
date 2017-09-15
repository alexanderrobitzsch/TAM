## File Name: tam_mml_person_maxscore.R
## File Version: 0.05
## File Last Change: 2017-04-27 20:52:50

tam_mml_person_maxscore <- function(resp, resp.ind=NULL)
{
	if ( is.null(resp.ind) ){
		resp.ind <- 1 - is.na(resp)
	}
	nstud <- nrow(resp)
	M1 <- apply( resp, 2 , max , na.rm=TRUE)
	M1 <- matrix(M1, nrow=nstud , ncol=ncol(resp) , byrow=TRUE)
	maxscore <- rowSums(  M1 * resp.ind )
	return(maxscore)	
}
