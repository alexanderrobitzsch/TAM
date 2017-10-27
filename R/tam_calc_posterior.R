## File Name: tam_calc_posterior.R
## File Version: 9.12



###########################################################
tam_calc_posterior <-
  function(rprobs , gwt , resp , nitems , 
           resp.ind.list , normalization = TRUE , 
           thetasamp.density = NULL , snodes = 0 , resp.ind=NULL,
		   avoid.zerosum=FALSE , logprobs=FALSE )
{   

# a0 <- Sys.time()		   

	fx <- gwt
	tsd <- NULL
	# calculate individual 'sampling weight'
    if ( snodes > 0 ){               
	  nstud <- nrow(gwt)
	  tsd <- matrix( thetasamp.density , nrow=nstud , ncol= ncol(gwt) , byrow=TRUE)
	  gwt <- gwt / tsd
	  gwt <- gwt / ncol(gwt)			
	  # gwt <- gwt / rowSums(gwt)
      swt <- fx <- gwt
    } 
# cat("vor calcfx") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1			
    nstud <- nrow(fx)
    storage.mode(resp) <- "integer"	
	fx0 <- fx	
    fx <- .Call("calcfx", fx, rprobs, resp.ind.list, resp)
# cat("nach calcfx") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1		
	
	if (avoid.zerosum ){	
		fxs <- rowSums( fx )
				m1 <- max( min( fxs[ fxs > 0 ] , na.rm=TRUE) , 1E-200 ) / 1E3 / ncol(fx)	
        ind <- which( fxs == 0 )
		if ( length(ind) > 0 ){
			fx[ (fxs == 0) , ] <- m1
		}
		fx[ is.na(fxs) , ] <- m1 
	}		
# cat("nach calcfx (2)") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1			
	rfx <- rowSums(fx)		
    if (normalization ){
        hwt <- fx / rfx 
	} else {   
		hwt <- fx 
	}		
    res <-  list("hwt" = hwt , "rfx" = rfx  )		
	res$fx1 <- fx / gwt	
    if ( snodes > 0 ){ 		
		res[["swt" ]] <- fx
		res$gwt <- gwt
	}
	res$tsd <- tsd
	#--- output
    return(res)
}
#####################################################################

calc_posterior.v2 <- tam_calc_posterior

# cat(" in  posterior rest") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1	    
