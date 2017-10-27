## File Name: tam_pv_mcmc_parameter_samples_correlation.R
## File Version: 0.08


tam_pv_mcmc_parameter_samples_correlation <- function(variance_samples, cor_index)
{
	NH <- max(cor_index$index2)
	NS <- nrow(variance_samples)
	D <- max(cor_index$dim1)
	cormat <- matrix(1, nrow=D, ncol=D)
	mat <- matrix(NA, nrow=NS, ncol=NH)	
	if (NH>0){
		for (hh in 1:NH){
			cor_index_hh <- cor_index[ cor_index$index2 == hh , ]
			dd1 <- cor_index_hh$dim1
			dd2 <- cor_index_hh$dim2
			ind1 <- cor_index[ ( cor_index$dim1 == dd1 ) & ( cor_index$dim2 == dd1 ) , ]$index
			ind2 <- cor_index[ ( cor_index$dim1 == dd2 ) & ( cor_index$dim2 == dd2 ) , ]$index
			mat[,hh] <- variance_samples[ , cor_index_hh$index ] /
						  sqrt( variance_samples[,ind1] * variance_samples[,ind2] )
			cor_hh <- mean(mat[,hh])
			cormat[ cor_index_hh$dim1 , cor_index_hh$dim2 ] <- cor_hh
			cormat[ cor_index_hh$dim2 , cor_index_hh$dim1 ] <- cor_hh
		}		
	}
	return(cormat)
}
