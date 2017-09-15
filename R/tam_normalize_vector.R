## File Name: tam_normalize_vector.R
## File Version: 0.01
## File Last Change: 2017-06-15 11:33:40

tam_normalize_vector <- function(x)
{
	x <- x / sum(x) 
	return(x)
}
