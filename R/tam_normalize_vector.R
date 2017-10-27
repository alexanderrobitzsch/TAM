## File Name: tam_normalize_vector.R
## File Version: 0.01

tam_normalize_vector <- function(x)
{
	x <- x / sum(x) 
	return(x)
}
