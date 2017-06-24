
#####################################################################
# compute F design matrix
tam_mml_3pl_compute_Fdes <- function( E , gammaslope , theta ){	  
	#*********
	#  SETUP: 3PL Structured latent class analysis
    #  design matrix E ... E[ii,cc,dd,pp] with loading parameters \gamma
    #  B[ii,cc,dd] = E[ii,cc,dd,] %*% gamma[]
    #  f[ii,cc,tt,kk] = \sum_dd E[ii,cc,dd,kk] * theta[jj,dd]
    #*********	   
	Ngam <- length(gammaslope)
	dimE <- dim(E)
	I <- dimE[1]
	maxK <- dimE[2]
	D <- dimE[3]
	TP <- nrow(theta)	   	   
	Fdes <- array( 0 , dim=c(I , maxK , TP , Ngam ) )
	ttheta <- t(theta)
    for (pp in 1:Ngam){
		for (cc in 1:maxK){
			if (D==1){		
				Fdes[,cc,,pp] <- E[ , cc , , pp , drop=FALSE] %*% ttheta
			}
			if (D >1){
				E.cc.pp <- E[ , cc , , pp  ]
				#***** Fdes <- array( 0 , dim=c(I , maxK , TP , Ngam ) )
				Fdes[,cc,,pp] <- E.cc.pp %*% ttheta
				# Fdes[,cc,,pp] <- E.cc.pp %*% ttheta
			}							
		}
	}					
    return(Fdes)
}

		
.mml.3pl.computeFdes <- tam_mml_3pl_compute_Fdes
		