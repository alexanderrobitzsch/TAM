## File Name: summary_tamaan_item_parameters.R
## File Version: 0.01
## File Last Change: 2017-09-15 09:45:51


#########################################################################
# item parameters
summary_tamaan_item_parameters <- function(object)
{
	cat("------------------------------------------------------------\n")		
	cat("Item Parameters -A*Xsi\n")
	obji <- object$item
	for (vv in seq(2,ncol(obji) ) ){ 
		obji[,vv] <- round( obji[,vv] , 3) 
	}
	print(obji)
	# print xsi parameters if 
	if( ! is.null( object$formulaA)  ){
		cat("\nItem Facet Parameters Xsi\n")
		obji <- object$xsi.facets
		for (vv in seq(3,ncol(obji) ) ){ 
			obji[,vv] <- round( obji[,vv] , 3) 
		}
		print(obji)
	}				
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
		obji <- object$xsi
		for (vv in seq(1,ncol(obji) ) ){ 
			obji[,vv] <- round( obji[,vv] , 3) 
		}
		print(obji)
	}
		
	cat("------------------------------------------------------------\n")		
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
		obji <- object$xsi
		for (vv in seq(1,ncol(obji) ) ){ 
			obji[,vv] <- round( obji[,vv] , 3) 
		}
		print(obji)
	}
	cat("\nGammaslope Parameters\n")
	obji <- object$gammaslope
	print(round(obji,3)  )   			
	cat("\nGuessing Parameters\n")
	obji <- object$item$guess
	names(obji) <- colnames(object$resp)
	print(round(obji,3))
}
###################################################################################					
		
