## File Name: tam_mml_person_SD_EAP.R
## File Version: 0.01


tam_mml_person_SD_EAP <- function( hwt, theta, EAP )
{
	N <- nrow(hwt)
	TP <- ncol(hwt)
	M1 <- matrix(as.vector(theta)^2, nrow=N , ncol=TP, byrow=TRUE)
	SD_EAP <- sqrt( rowSums( hwt * M1 ) - EAP^2 )	
	return(SD_EAP)
}
	
