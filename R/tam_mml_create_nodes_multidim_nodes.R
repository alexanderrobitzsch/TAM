## File Name: tam_mml_create_nodes_multidim_nodes.R
## File Version: 0.01
## File Last Change: 2017-09-14 19:10:17

tam_mml_create_nodes_multidim_nodes <- function(nodes, ndim)
{
	use_expand_grid <- TRUE
	if (ndim == 1){
		theta <- matrix( nodes, ncol=1)
		use_expand_grid <- FALSE
	}
	if (ndim > 1){
		if ( is.matrix(nodes) ){
			theta <- nodes
			use_expand_grid <- FALSE
		}
	}	
	if (use_expand_grid){
		theta <- as.matrix( expand.grid( as.data.frame( matrix( rep(nodes, ndim) , ncol = ndim ))))
	}	
	return(theta)
}
