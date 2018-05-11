## File Name: tam_latreg_se_quick_likelihood.R
## File Version: 0.03


tam_latreg_se_quick_likelihood <- function( gwt, like, thetawidth, snodes)
{
    hwt <- like * gwt
    loglike_num <- rowSums(hwt)
    loglike_sto <- hwt / rowSums(hwt)
    if ( snodes == 0 ){
        res <- log( loglike_num * thetawidth )
    } else {
        res <- log( loglike_sto )
    }
    return(res)
}
