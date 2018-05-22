## File Name: tam_compute_disp_progress.R
## File Version: 0.07


tam_compute_disp_progress <- function( ip, n_disp=10 )
{
    VP <- min( ip, n_disp )
    cat(paste( rep("*", VP), collapse=""))
    if (VP<10){
        disp_progress <- 1:ip
    } else {
        disp_progress <- 100* ( 1:ip ) / (ip+1)
        s_disp <- 100 / ( 2 *  n_disp )
        disp_progress <- sapply( seq(s_disp,100 - s_disp, n_disp), FUN=function(pp){
            which.min( abs( disp_progress - pp ) )[1] }
        )
    }
    cat("|\n|")
    return(disp_progress)
}
