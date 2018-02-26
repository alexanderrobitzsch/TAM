## File Name: tam_mml_se_quick_difference_quotient.R
## File Version: 0.01


tam_mml_se_quick_difference_quotient <- function(ll, h, pweights )
{
	sum( pweights * ( ll[,2] + ll[,3] - 2*ll[,1] )/h^2 )
}
