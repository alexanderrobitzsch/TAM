
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