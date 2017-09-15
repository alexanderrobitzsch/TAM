## File Name: tam_pv_summary_covariance.R
## File Version: 0.03
## File Last Change: 2017-08-15 18:40:47

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
