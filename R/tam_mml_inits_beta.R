## File Name: tam_mml_inits_beta.R
## File Version: 0.07

tam_mml_inits_beta <- function(Y, formulaY, dataY, G, group, groups, nstud,
		pweights, ridge, beta.fixed, xsi.fixed, constraint, ndim, beta.inits)
{
	nullY <- is.null(Y)
    # beta inits
    # (Y'Y)
    if ( ! is.null( formulaY ) ){
      formulaY <- stats::as.formula( formulaY )
      Y <- stats::model.matrix( formulaY , dataY )[,-1]   # remove intercept
      nullY <- FALSE
    }	
    # labeling Y
    if (! nullY){
		Y <- as.matrix(Y)
		nreg <- ncol(Y)
		if ( is.null( colnames(Y) ) ){
			colnames(Y) <- paste("Y" , 1:nreg , sep="")
		}
		if ( ! nullY ){ 		
			Y <- cbind(1,Y)          #add a "1" column for the Intercept
			colnames(Y)[1] <- "Intercept"
		}
	} else {
		Y <- matrix( 1 , nrow=nstud , ncol=1 ) 
		nreg <- 0
    }
    if ( G > 1 & nullY ){	
		Y <- matrix( 0 , nstud , G )
		colnames(Y) <- paste("group" , groups , sep="")
		for (gg in 1:G){ 
			Y[,gg] <- 1*(group==gg) 
		}
		nreg <- G - 1
    }

    W <- t(Y * pweights) %*% Y
    if (ridge > 0){ 
		diag(W) <- diag(W) + ridge 
	}
	# YYinv <- solve( W )	
    YYinv <- MASS::ginv( W )	
	    
    #initialise regressors
    if ( is.null(beta.fixed) & (  is.null(xsi.fixed) ) & ( constraint=="cases") ){
		beta.fixed <- matrix( c(1,1,0) , nrow= 1) 
		if (  ndim > 1){ 
			for ( dd in 2:ndim){
				beta.fixed <- rbind( beta.fixed , c( 1 , dd , 0 ) )
			}
		}
	}    
    #****
    if( ! is.matrix(beta.fixed) ){
		if ( ! is.null(beta.fixed) ){
			if ( ! beta.fixed   ){ 
				beta.fixed <- NULL 
			}
		}
    }

    #-- inits beta
    beta <- matrix(0, nrow = nreg+1 , ncol = ndim)  
    if ( ! is.null( beta.inits ) ){ 
		beta[ beta.inits[,1:2] ] <- beta.inits[,3]
    }	

	# --- OUTPUT
	res <- list(Y=Y, nullY=nullY, formulaY=formulaY, nreg=nreg, W=W, YYinv=YYinv,
				beta.fixed=beta.fixed, beta=beta)
	return(res)
}
    
