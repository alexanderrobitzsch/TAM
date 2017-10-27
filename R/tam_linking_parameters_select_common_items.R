## File Name: tam_linking_parameters_select_common_items.R
## File Version: 0.03

tam_linking_parameters_select_common_items <- function(out, items_sel, names_suffix=NULL)
{
	#-- A
	A <- out$A
	A <- A[ items_sel ,,,drop=FALSE ]
	#-- B
	B <- out$B
	B <- B[ items_sel ,, , drop=FALSE]
	#-- AXsi
	AXsi <- out$AXsi
	AXsi <- AXsi[ items_sel , , drop=FALSE]
	#-- guess
	guess <- out$guess
	if ( ! is.null(guess) ){
		guess <- guess[ items_sel ]
	}
	#-- distribution
	M <- out$M
	SD <- out$SD
	#--- OUTPUT
	res <- list(B=B, AXsi=AXsi, guess=guess, M=M, SD=SD)
	if ( ! is.null(names_suffix) ){
		names(res) <- paste0( names(res) , names_suffix )
	}
	return(res)
}
