## File Name: tam_jml_proc_abilities.R
## File Version: 0.02


tam_jml_proc_abilities <- function(theta, pweights, B)
{
    ndim <- dim(B)[3]
    if (is.vector(theta)){
        M <- weighted_mean(x=theta, w=pweights)
        SD <- weighted_sd(x=theta, w=pweights)
    } else {
        M <- NULL
        SD <- NULL
    }
    res <- list(M=M, SD=SD, ndim=ndim)
}
