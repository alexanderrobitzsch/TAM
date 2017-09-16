## File Name: summary_tamaan_item_parameters.R
## File Version: 0.05
## File Last Change: 2017-09-16 12:18:44

#########################################################################
# item parameters
summary_tamaan_item_parameters <- function(object)
{
	cat("------------------------------------------------------------\n")		
	cat("Item Parameters -A*Xsi\n")
	obji <- object$item
	res <- tam_round_data_frame_print(obji=obji, from=2, to=ncol(obji), digits=3)
	# print xsi parameters if 
	if( ! is.null( object$formulaA)  ){
		cat("\nItem Facet Parameters Xsi\n")
		obji <- object$xsi.facets
		res <- tam_round_data_frame_print(obji=obji, from=3, to=ncol(obji), digits=3)
	}				
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
		obji <- object$xsi
		res <- tam_round_data_frame_print(obji=obji, from=1, to=ncol(obji), digits=3)		
	}
		
	cat("------------------------------------------------------------\n")		
	if (( object$maxK > 2 ) | ( object$printxsi) ){
		cat("\nItem Parameters Xsi\n")
		obji <- object$xsi
		res <- tam_round_data_frame_print(obji=obji, from=1, to=ncol(obji), digits=3)
	}
	#-- gammaslope
	cat("\nGammaslope Parameters\n")
	obji <- object$gammaslope
	res <- tam_round_data_frame_print(obji=obji, digits=3)
	
	#-- guessing
	cat("\nGuessing Parameters\n")
	obji <- object$item$guess
	names(obji) <- colnames(object$resp)
	res <- tam_round_data_frame_print(obji=obji, digits=3)
}
###################################################################################					
		
