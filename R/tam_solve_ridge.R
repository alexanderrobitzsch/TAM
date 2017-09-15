## File Name: tam_solve_ridge.R
## File Version: 0.01
## File Last Change: 2017-06-01 17:04:10

tam_solve_ridge <- function(x, ridge)
{
	diag(x) <- diag(x) + ridge
	return( solve(x) )
}
