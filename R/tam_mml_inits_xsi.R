
tam_mml_inits_xsi <- function(A, resp.ind, ItemScore, xsi.inits, xsi.fixed ,
		est.xsi.index, pweights, xsi.start0, xsi, resp, addnumb = .5 )
{

    # starting values for xsi
    maxAi <-  - (apply(-(A) , 3 , rowMaxs , na.rm=TRUE) )  
    personMaxA <- resp.ind %*% maxAi
	ItemMax <- crossprod( personMaxA , pweights ) 

	#-- needed for 2PL estimation maximum score in resp, equal categories?  
	maxscore.resp <- apply( resp , 2 , max )
	if ( ncol(resp)>1){ 
		sd.maxscore.resp <- stats::sd(maxscore.resp)
	} else { 
		sd.maxscore.resp <- 0 
	}
	equal.categ <- if( sd.maxscore.resp > 1E-6 ){ FALSE } else { TRUE  }	
	
	#--- inits for xsi
    xsi[est.xsi.index] <- - log(abs(( ItemScore[est.xsi.index]+addnumb)/
                                      (ItemMax[est.xsi.index]-ItemScore[est.xsi.index]+addnumb) ) )
    # starting values of zero
    if( xsi.start0 ){ 
		xsi <- 0*xsi 
	}
    
    #log of odds ratio of raw scores  
    if ( ! is.null(xsi.inits) ){    
		xsi[ xsi.inits[,1] ] <- xsi.inits[,2]
    }
    if ( ! is.null( xsi.fixed ) ){ 
		xsi[ xsi.fixed[,1] ] <- xsi.fixed[,2] 
	}

	#--- OUTPUT
	res <- list(xsi=xsi, personMaxA=personMaxA, ItemMax=ItemMax, equal.categ=equal.categ)
	return(res)	
}