## File Name: tam_mml_person_EAP.R
## File Version: 0.03


tam_mml_person_EAP <- function( hwt, theta )
{
    N <- nrow(hwt)
    TP <- ncol(hwt)
    M1 <- matrix(as.vector(theta), nrow=N, ncol=TP, byrow=TRUE)
    EAP <- rowSums( hwt * M1 )
    return(EAP)
}

