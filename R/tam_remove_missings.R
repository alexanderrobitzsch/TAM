## File Name: tam_remove_missings.R
## File Version: 0.02

tam_remove_missings <- function( dat , items, elim_items=TRUE, elim_persons=TRUE )
{
	cn <- colnames(dat)
	non_items <- setdiff( cn , items )
	resp <- dat[, items]
	if (elim_items){
		resp <- resp[ , colMeans( is.na(resp) ) < 1 ]
		items <- colnames(resp)
		vars <- c( non_items , items )
		vars <- intersect( cn , vars )
		dat <- dat[, vars ]
	}
	if (elim_persons){
		ind <- which( rowMeans( is.na(resp) ) < 1 )
		resp <- resp[ ind, ]
		dat <- dat[ ind , ]
	}
	#--- output
	res <- list( resp = resp , dat = dat , items=items )
	return(res)
}
