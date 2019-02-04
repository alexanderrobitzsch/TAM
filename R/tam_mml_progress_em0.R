## File Name: tam_mml_progress_em0.R
## File Version: 0.05

tam_mml_progress_em0 <- function(progress, iter, disp, print_estep=TRUE)
{
    if (progress){
        cat(disp)
        cat("Iteration", iter, "   ", paste( Sys.time() ) )
        if (print_estep){
            cat("\nE Step\n")
        }
        utils::flush.console()
    }
}
