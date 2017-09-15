## File Name: tam_mml_3pl_inits_group.R
## File Version: 0.01
## File Last Change: 2017-05-25 15:27:28

tam_mml_3pl_inits_group <- function(group, ndim, G, variance.inits, groups)
{
	var.indices <- NULL
    # group indicators for variance matrix
    if ( ! is.null(group) ){ 
		var.indices <- rep(1,G)
		for (gg in 1:G){
			var.indices[gg] <- which( group == gg )[1]				
		}
		if ( is.null( variance.inits ) ){
			variance <- array( 0 , dim=c(G,ndim,ndim) )
			for (gg in 1:G){
				variance[gg,,] <- diag(ndim)
			}
		}	  
    }
	#--- OUTPUT
	res <- list(G=G, groups=groups, group=group, var.indices=var.indices)
	return(res)	
}
