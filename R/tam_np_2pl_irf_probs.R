## File Name: tam_np_2pl_irf_probs.R
## File Version: 0.04


tam_np_2pl_irf_probs <- function(x, par0, index, desmat, ...)
{
    par0[index] <- x
    pred <- desmat %*% par0
    TP <- nrow(desmat)
    p1 <- as.vector( stats::plogis(pred) )
    probs <- matrix(0, nrow=2, ncol=TP)
    probs[2,] <- p1
    probs[1,] <- 1 - p1
    return(probs)
}

