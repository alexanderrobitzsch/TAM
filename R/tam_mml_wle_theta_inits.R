## File Name: tam_mml_wle_theta_inits.R
## File Version: 0.03
## File Last Change: 2017-09-15 17:32:46


tam_mml_wle_theta_inits <- function(WLE, adj, nitems, maxK, resp, resp.ind, B,
		ndim )
{	
	nstud <- nrow(resp)
	#*** readjust in case of WLE
	if (WLE){
		adj <- 0
	}
    col.index <- rep( 1:nitems , each = maxK )
    cResp <- resp[ , col.index  ]*resp.ind[ , col.index ]
    cResp <- 1 * t( t(cResp) == rep(0:(maxK-1), nitems) )
    cB <- t( matrix( aperm( B , c(2,1,3) ) , nrow = dim(B)[3] , byrow = TRUE ) )
    cB[is.na(cB)] <- 0
    
    #Compute person sufficient statistics (total score on each dimension)
    PersonScores <- cResp %*% cB
    
    #Compute possible maximum score for each item on each dimension
    maxBi <- apply(B , 3 , tam_rowMaxs , na.rm = TRUE)
    
    #Compute possible maximum score for each person on each dimension
    PersonMax <- resp.ind %*% maxBi
    PersonMax[ PersonMax == 0 ] <- 2 * adj
    
    #Adjust perfect scores for each person on each dimension	
    PersonScores[PersonScores==PersonMax] <- PersonScores[PersonScores==PersonMax] - adj
    
    #Adjust zero scores for each person on each dimension
    PersonScores[PersonScores==0] <- PersonScores[PersonScores==0] + adj
    	
    #Initialise theta (WLE) values for all students
    theta <- log((PersonScores+.5)/(PersonMax-PersonScores+1)) #log of odds ratio of raw score    
	
    converge <- FALSE
    Miter <- 0
    BB <- array(0, dim=c(nitems,maxK,ndim,ndim))
    BBB <- array(0, dim=c(nitems,maxK,ndim)) 
    for (i in 1:nitems) {
		for (k in 1:maxK) {
			BB[i,k,,] <- B[i,k,] %*% t(B[i,k,])
			BBB[i,k,] <- BB[i,k,,] %*% B[i,k,]
		}
    }
    increment <- array(0, dim=c(nstud,ndim))
    old_increment <- 3 + increment
	
	#--- OUTPUT
	res <- list( adj=adj, PersonScores=PersonScores, PersonMax=PersonMax,
					theta=theta, converge=converge, Miter=Miter, BB=BB,
					BBB=BBB, increment=increment, old_increment=old_increment)
	return(res)
}
