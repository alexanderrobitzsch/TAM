## File Name: tam_linking_extract_parameters_trait_distribution.R
## File Version: 0.01
## File Last Change: 2017-06-22 14:28:03

tam_linking_extract_parameters_trait_distribution <- function(tamobj)
{
	class_tamobj <- class(tamobj)
	G <- tamobj$G
	group <- tamobj$group
	M <- as.vector( unlist(tamobj$beta) )
	variance <- tamobj$variance
	if ( class_tamobj %in% c("tam.mml", "tam.mml.2pl", "tam.mml.mfr") ){
		if (G > 1){
			variance_group <- tam_aggregate( variance, group, mean=TRUE)
			SD <- sqrt( variance_group[,2] )
		} else {
			SD <- sqrt(variance[1,1])
		}
	} else {
		SD <- sqrt( as.vector( unlist( variance ) ) )
	}		
	#--- output
	res <- list( class_tamobj=class_tamobj, G=G, group=group, M=M, SD=SD)
	return(res)
}
