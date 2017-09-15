## File Name: tam_mml_3pl_computeB.R
## File Version: 0.02
## File Last Change: 2017-06-23 12:45:06


#############################################################
# faster function for computation of item loadings
tam_mml_3pl_computeB <- function( Edes , gammaslope , E ){
	B <- tam_mml_3pl_compute_B_rcpp( Edes , gammaslope , dim(E) )$B
	B <- array( B , dim(E)[1:3] )
	return(B)
}

.mml.3pl.computeB.v2 <- tam_mml_3pl_computeB


#########################################################
# calculation of B matrix
.mml.3pl.computeB <- function( E , gammaslope ){
    dimE <- dim(E)
	I <- dimE[1]
	maxK <- dimE[2]
	D <- dimE[3]
	B <- array( 0 , dim= c(I , maxK , D ) )
	for (dd in 1:D){
		for (cc in 1:maxK){
			B[ , cc,dd] <- E[,cc,dd,] %*% gammaslope
		}
	}			
	return(B)
}
