## File Name: tam_mml_3pl_create_notA.R
## File Version: 0.01
## File Last Change: 2017-05-08 17:20:39


####################################################
# create a dummy A matrix
tam_mml_3pl_create_notA <- function( E , notA ){  
    dimE <- dim(E)
	A <- array( 0 , dim=c(dimE[1] , dimE[2] , 2 ) )
    xsi.fixed <- cbind( c(1,2) , 0 )
	res <- list("A"=A , "xsi.fixed" = xsi.fixed )
	return(res)
}
####################################################################	
		

.mml.3pl.create.notA <- tam_mml_3pl_create_notA
		
