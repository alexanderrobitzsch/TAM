
tam_solve_ridge <- function(x, ridge)
{
	diag(x) <- diag(x) + ridge
	return( solve(x) )
}