## File Name: tam_interval_index.R
## File Version: 0.01
## File Last Change: 2017-05-25 19:47:20

		
# 'interval_index' searches an index when a frequency is exceeded
# -> used in plausible value imputation
tam_interval_index <- function(matr, rn){ 
	res <- interval_index_C( matr, rn )
	res <- res + 1
	return(res)
}		

interval_index <- tam_interval_index
