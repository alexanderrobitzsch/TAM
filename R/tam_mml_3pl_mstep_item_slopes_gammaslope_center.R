## File Name: tam_mml_3pl_mstep_item_slopes_gammaslope_center.R
## File Version: 0.01
## File Last Change: 2017-05-02 12:51:43


################################################################################			
# centering gammaslope vector
tam_mml_3pl_mstep_item_slopes_gammaslope_center <- function( gammaslope , 
		gammaslope.center.index  ,	gammaslope.center.value  )
{														
	if ( ! is.null( gammaslope.center.index ) ){
		M <- max( gammaslope.center.index )
		for (mm in 1:M){
			ind.mm <- which( gammaslope.center.index == mm )
			IM <- length(ind.mm)
			rmm <- gammaslope[ ind.mm ] - mean( gammaslope[ind.mm] )
			gammaslope[ ind.mm ] <- gammaslope.center.value[mm] / IM + rmm
		}
	}
	return(gammaslope)
}
###########################################################


.mml.3pl.gammaslope.center <- tam_mml_3pl_mstep_item_slopes_gammaslope_center
