## File Name: tamaan_3pl_lca_extract_lcaprobs.R
## File Version: 0.04


##########################################################
# extract LCA probabilities
tamaan_3pl_lca_extract_lcaprobs <- function(res)
{
	rprobs <- res$rprobs
	resp <- res$resp
	rpdim <- dim(rprobs)
	ncat <- rpdim[2]
	I <- rpdim[1]
	TP <- rpdim[3]
	obji <- NULL
	for (hh in 1:ncat){
		obji <- rbind( obji , rprobs[,hh,] )
	}
	colnames(obji) <- class_labels <- paste0("Class", 1:TP )
	obji <- data.frame( "item" = rep( colnames(resp) , ncat) ,
						"itemno" = rep( 1:I , ncat) , "Cat" =
						rep(1:ncat , each=I)-1 , obji )
	obji <- obji[ order( obji$itemno ) , ]		
	rownames(obji) <- NULL
	
	#--- average probabilities	
	a1 <- aggregate( obji$Cat * obji[,class_labels] , list(obji$item) , sum , na.rm=TRUE)
	a2 <- aggregate( obji$itemno, list(obji$item) , mean , na.rm=TRUE)
	lca_M <- data.frame( item = a1[,1] , itemno = a2[,2] , a1[,-1])
	colnames(lca_M)[-c(1,2)] <- class_labels
	
	#--- output
	res0 <- list( lcaprobs = obji, lca_M = lca_M )
	return(res0)
}
#######################################################

.extract.lcaprobs <- tamaan_3pl_lca_extract_lcaprobs
