## File Name: tam_mml_progress_em0.R
## File Version: 0.01

tam_mml_progress_em0 <- function(progress, iter, disp)
{
	if (progress){ 
		cat(disp)	
		cat("Iteration" , iter , "   " , paste( Sys.time() ) )
		cat("\nE Step\n") ; flush.console()
	}
}
