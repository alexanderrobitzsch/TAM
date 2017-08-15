
tam_pv_summary_covariance <- function( obji, label, digits=3)
{
	cat("------------------------------------------------------------\n")		
	cat( label , "\n\n")
	G <- length(obji)
	for (gg in 1:G){
		cat("Group" , gg , "\n")
		print( round( obji[[gg]],digits) )
		cat("\n")
	}
}