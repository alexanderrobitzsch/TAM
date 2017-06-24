
tam_aggregate <- function(x, group, mean=FALSE, na.rm=TRUE)
{
	g1 <- rowsum(x=x, group=group, na.rm=na.rm)
	if (mean){
		g2 <- rowsum(x=1+0*x, group=group, na.rm=na.rm)
		g1 <- g1 / g2
	}
	ng1 <- as.numeric(paste(rownames(g1)))
	g1 <- cbind( ng1 , g1 )	
	return(g1)
}