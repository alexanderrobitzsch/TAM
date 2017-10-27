## File Name: tam_solve_ridge.R
## File Version: 0.01

tam_solve_ridge <- function(x, ridge)
{
	diag(x) <- diag(x) + ridge
	return( solve(x) )
}
