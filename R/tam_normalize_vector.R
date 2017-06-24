
tam_normalize_vector <- function(x)
{
	x <- x / sum(x) 
	return(x)
}