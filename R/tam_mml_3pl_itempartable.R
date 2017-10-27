## File Name: tam_mml_3pl_itempartable.R
## File Version: 9.04


##################################################
# create table of item parameters
tam_mml_3pl_itempartable <- function( resp , maxK , AXsi , B , ndim ,
			resp.ind , rprobs,n.ik,pi.k , guess , est.guess ,
			order.items=FALSE)
{				
	item1 <- data.frame( "item" = colnames(resp) )
	item1$N <- colSums(resp.ind )
	item1$M <- colSums( resp.ind * resp , na.rm=TRUE) / colSums( resp.ind )
	#****
	# Item fit
	# probs ... [ classes , items , categories ]
	probs <- aperm( rprobs , perm=c(3,1,2))
	pi.k <- matrix( pi.k , ncol=1 )
    if ( is.null( est.guess) ){ est.guess <- 0 }
	item1$est.guess <- est.guess
	item1$guess <- guess
	for (kk in 1:(maxK-1)){
		item1[ , paste0("AXsi_.Cat" , kk) ] <- - AXsi[,kk+1]
	}
	for (kk in 1:(maxK-1)){
		for (dd in 1:ndim){
			item1[ , paste0("B.Cat" , kk,".Dim",dd) ] <- B[,kk+1,dd]
		}
	}				
    item1 <- item1[ item1$N > 0 , ]	
	if ( order.items ){
		item1 <- item1[ order( paste( item1$item)) , ]		
	}
	rownames(item1) <- NULL
	#*** substitute -99 by missing
	item1[ item1 == -99 ] <- NA		
	return(item1)
}
#######################################################


.mml.3pl.TAM.itempartable <- tam_mml_3pl_itempartable
