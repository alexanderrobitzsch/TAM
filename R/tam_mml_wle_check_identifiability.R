## File Name: tam_mml_wle_check_identifiability.R
## File Version: 0.03

tam_mml_wle_check_identifiability <- function(B)
{
    # check identifiability
    dimB <- dim(B)
    D <- dimB[3]
    I <- dimB[1]
    K <- dimB[2]
    identM <- matrix( 0 , nrow=I , ncol=D )
    for ( kk in 1:K){
        identM <- identM + 1 * ( B[,kk,] != 0 )
    }
    betweenload <- 0*identM
    sumloads <- rowSums( identM > 0 )
    ind <- which( sumloads == 1 )
    if (length(ind) > 0 ){
        betweenload[ ind , ] <- identM[ ind , ]
    }
    loaddim <- colSums(betweenload)
    if ( min(loaddim) == 0 ){
        cat("\n * Not all dimensions do have items with simple loading structure.\n")
        cat(" * Maybe the WLE is not identified (i.e. estimable).\n")
        cat(" * Please proceed with caution.\n")
    }
}
