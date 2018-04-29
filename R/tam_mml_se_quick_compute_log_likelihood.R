## File Name: tam_mml_se_quick_compute_log_likelihood.R
## File Version: 0.03

tam_mml_se_quick_compute_log_likelihood <- function( like0, thetawidth, snodes )
{
    if ( snodes == 0 ){
        res <- log( rowSums( like0 * thetawidth ) )
    } else {
        res <- log( rowSums( like0  ) )
    }
    return(res)
}
