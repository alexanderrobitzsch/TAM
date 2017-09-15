## File Name: tamaanify.tam.mml.3pldesignMatrices.R
## File Version: 9.02
## File Last Change: 2017-01-24 18:13:52



###########################################################
# design matrices for estimation with tam.mml.3pl method
tamaanify.tam.mml.3pl.designMatrices <- function(res){	

	anlist <- res$ANALYSIS.list
	res <- switch( anlist$type ,
			"LCA" = tamaanify.tam.mml.3pl.designMatrices.LCA(res) ,
			"LOCLCA" = tamaanify.tam.mml.3pl.designMatrices.LOCLCA(res) ,
			"OLCA" = tamaanify.tam.mml.3pl.designMatrices.OLCA(res) ,
			"TRAIT" = tamaanify.tam.mml.3pl.designMatrices.TRAIT(res) ,
			"MIXTURE" = tamaanify.tam.mml.3pl.designMatrices.MIXTURE(res)
						)
	

	
	# output
	return(res)
		}
