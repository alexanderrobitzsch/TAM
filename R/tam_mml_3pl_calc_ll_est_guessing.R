## File Name: tam_mml_3pl_calc_ll_est_guessing.R
## File Version: 0.02
## File Last Change: 2017-01-24 17:13:51

tam_mml_3pl_calc_ll_est_guessing <- 
	function( n0ij , n1ij , probs, eps )
{
	l1 <- rowSums( n0ij * log( probs[,1,] + eps ) + n1ij * log( probs[,2,] + eps ) )
	return(l1)
}
