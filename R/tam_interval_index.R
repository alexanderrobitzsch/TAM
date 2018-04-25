## File Name: tam_interval_index.R
## File Version: 0.03

		
# 'interval_index' searches an index when a frequency is exceeded
# -> used in plausible value imputation
tam_interval_index <- function(matr, rn){ 
	res <- tam_rcpp_interval_index( MATR=matr, RN=rn )
	res <- res + 1
	return(res)
}		

interval_index <- tam_interval_index
