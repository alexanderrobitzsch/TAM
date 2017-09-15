## File Name: tam_mml_person_EAP.R
## File Version: 0.01
## File Last Change: 2017-04-27 20:21:05


tam_mml_person_EAP <- function( hwt, theta )
{
	N <- nrow(hwt)
	TP <- ncol(hwt)
	M1 <- matrix(as.vector(theta), nrow=N , ncol=TP, byrow=TRUE)
	EAP <- rowSums( hwt * M1 )
	return(EAP)
}
	
