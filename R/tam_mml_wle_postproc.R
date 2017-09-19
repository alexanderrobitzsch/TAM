## File Name: tam_mml_wle_postproc.R
## File Version: 0.17
## File Last Change: 2017-09-19 16:06:34

tam_mml_wle_postproc <- function(ndim, err_inv, theta, pid, resp.ind,
	PersonScores, PersonMax, adj, WLE, rprobsWLE, output.prob, progress,
	pweights, CALL, B, score.resp)
{
    #standard errors of theta estimates
    if (ndim == 1) {
		error <- apply(err_inv,1,sqrt) 
    } else {    
		error <- aperm(apply(sqrt(err_inv),1,diag), c(2,1))
    }    
    # The output contains 
    #   Person Scores on the test, by dimension
    #   Person possible maximum score, by dimension (Each person could take 
    #    different items, so possible maximum could vary)
    #   WLE or MLE estimate, by dimension
    #   Standard errors of WLE/MLE estimates, by dimension
    
	dimlabels <- substring( 100+1:ndim , 2)
	
    if ( ndim> 1){
		colnames(error) <- paste0("error.Dim" , dimlabels )
    }
    res <- data.frame( "pid" = pid , 
                       "N.items" = rowSums(resp.ind) , 
                       "PersonScores" = PersonScores, 
                       "PersonMax" = PersonMax, "theta" = theta , error )
    
    if (ndim==1){ 
		colnames(res)[4:5] <- c("PersonMax" , "theta") 
	}
    if (ndim>1){  
		colnames(res)[ 1:ndim + 2] <- paste0("PersonScores.Dim" , dimlabels )	
		ind <- grep( "theta" , colnames(res) )	
		colnames(res)[ind] <- paste0("theta.Dim" , dimlabels )	
    }
    ####################
    # correct personMax set theta and standard error to missing		
    # if there are no observations on one dimension
    ind1 <- grep("PersonMax" , colnames(res))
    check1 <- ( res[ , ind1 , drop=FALSE] == 2*adj )
    ind2 <- grep("theta" , colnames(res))
    D <- length(ind1)
    for (ii in 1:D){
		res[ check1[,ii] , ind2[ii] ] <- NA
    }
    ind2 <- grep("error" , colnames(res))
    for (ii in 1:D){
		res[ check1[,ii] , ind2[ii] ] <- NA
    }
    #--- WLE reliability and average error variance
	M_sq_error <- rep(NA,ndim)
	names(M_sq_error) <- paste0("Dim",1:ndim)
	WLEvar <- WLEM <- M_sq_error
	if (WLE){ 
		w1 <- "WLE" 
	} else { 
		w1 <- "MLE" 
	}	
    if ( ndim==1 ){
		ind <- which( res$N.items > 0 )	  
		WLE.rel <- WLErel(theta=theta, error=error, w=pweights, select=ind)	  
		if (progress){
			cat("----\n" , w1 ,"Reliability =" , round(WLE.rel,3) ,"\n" )
		}
		res$WLE.rel <- rep( WLE.rel , nrow(res) )
		M_sq_error[1] <- weighted_mean( error[ind]^2 , pweights[ind] )
		WLEM[1] <- weighted_mean( theta[ind], pweights[ind] )
		WLEvar[1] <- weighted_var( theta[ind], pweights[ind] )
    }
    if ( ndim>1 ){
		cat("\n-------\n")
		for (dd in 1:ndim){
		    dimlabel_dd <- dimlabels[dd]
			ind1 <- paste0("theta.Dim" , dimlabel_dd )
			ind2 <- paste0("error.Dim" , dimlabel_dd )
			h1 <- WLErel( theta=res[,ind1] , error=res[,ind2] , w = pweights )
			res[ , paste0("WLE.rel.Dim" , dimlabel_dd ) ] <- h1
			if (progress){
				cat(paste0(w1 , " Reliability (Dimension" , dd , ") = " , round(h1,3) ) , "\n" )
			}
			M_sq_error[dd] <- weighted_mean( res[,ind2]^2 , pweights)
			WLEM[dd] <- weighted_mean( res[,ind1] , pweights)
			WLEvar[dd] <- weighted_var( res[,ind1], pweights )
		}
    }				
			
	#--- check identifiability
	res0 <- tam_mml_wle_check_identifiability(B=B)				
	res <- as.data.frame(res)			
	attr(res,"ndim") <- ndim
	attr(res,"nobs") <- nrow(res)
	attr(res,"M_sq_error") <- M_sq_error
	attr(res,"WLEvar") <- WLEvar
	attr(res,"WLEM") <- WLEM
	#--- collect reliabilities
	i1 <- grep( "WLE.rel" , colnames(res), fixed = TRUE )
    if (ndim==1){
		attr(res,"WLE.rel") <- res[[i1]][1]
	} else {
		v1 <- as.numeric(res[1,i1])
		names(v1) <- colnames(res)[i1]
		attr(res,"WLE.rel") <- v1	
	}	
	attr(res,"call") <- CALL
	class(res) <- c("tam.wle","data.frame")		
	if (output.prob){
		 res <- as.list(res)
	     res$probs <- rprobsWLE
	}	
	#--- OUTPUT
	return(res)
}
