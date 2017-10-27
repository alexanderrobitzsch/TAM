## File Name: tam_mml_inits_groups.R
## File Version: 0.02

tam_mml_inits_groups <- function( group )
{
	var.indices <- NULL
    # group indicators for variance matrix
    if ( ! is.null(group) ){ 
		groups <- sort(unique(group))
		G <- length(groups)
		group <- match( group , groups )
		var.indices <- rep(1,G)
		for (gg in 1:G){
			var.indices[gg] <- which( group == gg )[1]				
		}
	} else { 
		G <- 1 
		groups <- NULL
    }
	#--- OUTPUT
	res <- list(G=G, groups=groups, group=group, var.indices=var.indices)
	return(res)
}
