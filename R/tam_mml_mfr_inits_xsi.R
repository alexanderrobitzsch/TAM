## File Name: tam_mml_mfr_inits_xsi.R
## File Version: 0.02
## File Last Change: 2017-05-25 16:12:11

tam_mml_mfr_inits_xsi <- function( gresp.noStep.ind, col.index, cA , pweights , 
		xsi, xsi.start0, resp, A, xsi.inits, xsi.fixed, ItemScore, est.xsi.index, addnumb=.5 )
{
    # starting values for xsi
    gresp.ind.tmp <- gresp.noStep.ind[ , col.index  ]
	ItemMax <- crossprod(gresp.ind.tmp %*% cA , pweights )
    ItemMax <- as.vector( t( colSums( gresp.ind.tmp * pweights ) ) %*% cA )    
        
    xsi[est.xsi.index] <- - log(abs(( ItemScore[est.xsi.index]+addnumb)/
                                      (ItemMax[est.xsi.index]-ItemScore[est.xsi.index]+addnumb) ) )							  
    # starting values of zero
    if( xsi.start0 == 1){ 
		xsi <- 0*xsi 
	}
    if( xsi.start0 == 2){ 
		ind1 <- which( dimnames(A)[[3]] %in% colnames(resp) )
		ind2 <- which( dimnames(A)[[3]] %in% paste0( "step" ,1:9) )
		ind3 <- setdiff( seq(1,length(xsi) ) , union(ind1,ind2) )
		xsi[ind3] <- 0
	}
					        
    #log of odds ratio of raw scores  
    xsi[ is.na(xsi) ] <- 0
    if ( ! is.null(xsi.inits) ){  
		xsi[ xsi.inits[,1] ] <- xsi.inits[,2]			
    }
    if ( ! is.null(xsi.fixed) ){   
		xsi[ xsi.fixed[,1] ] <- xsi.fixed[,2] 
	}		
	#--- OUTPUT
	res <- list(xsi=xsi, ItemMax=ItemMax)
	return(res)	
}	
