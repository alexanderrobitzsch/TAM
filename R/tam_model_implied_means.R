## File Name: tam_model_implied_means.R
## File Version: 0.02


tam_model_implied_means <- function(mod)
{
    hwt <- mod$hwt
    pweights <- mod$pweights
    rprobs <- mod$rprobs
    resp <- mod$resp
    I <- dim(rprobs)[1]
    K <- dim(rprobs)[2]
    TP <- dim(rprobs)[3]
    M_implied <- rep(0,I)
    names(M_implied) <- colnames(resp)
    N <- nrow(hwt)
    W <- sum(pweights)

    for (ii in 1:I){
        for (uu in 2:K){
            rpr <- matrix( rprobs[ii,uu,], nrow=N, ncol=TP, byrow=TRUE)
            M_implied[ii] <- M_implied[ii] + sum( rpr*(uu-1)*pweights*hwt, na.rm=TRUE)
        }
        M_implied[ii] <- M_implied[ii] / W
    }
    return(M_implied)
}

