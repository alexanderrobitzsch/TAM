## File Name: tam_mml_3pl_calc_ll.R
## File Version: 0.03


tam_mml_3pl_calc_ll <- function( n.ik , probs, eps )
{
    maxK <- dim(n.ik)[2]
    probs <- probs + eps
    l1 <- 0
    for (kk in 1:maxK){
        l1 <- l1 + sum( n.ik[,kk,,drop=FALSE] * log( probs[,kk,,drop=FALSE] ) )
    }
    return(l1)
}
