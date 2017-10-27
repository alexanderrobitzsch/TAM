## File Name: tam_normalize_matrix_rows.R
## File Version: 0.01

# normalizing rows in a matrix
# such that each row sums to one
tam_normalize_matrix_rows <- function(x)
{
	x <- x / rowSums( x ) 
	return(x)
}
