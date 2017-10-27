## File Name: tam_outer.R
## File Version: 0.04

tam_outer <- function(x,y, op="*")
{
	Z <- NULL
	N1 <- length(x)
	N2 <- length(y)
	xM <- matrix(x, nrow=N1 , ncol=N2)
	yM <- tam_matrix2(y, nrow=N1 , ncol=N2)
	if (op=="*"){
		Z <- xM * yM
	}
	if (op=="+"){
		Z <- xM + yM
	}
	if (op=="-"){
		Z <- xM - yM
	}
	return(Z)
}
