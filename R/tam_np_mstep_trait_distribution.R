## File Name: tam_np_mstep_trait_distribution.R
## File Version: 0.13

tam_np_mstep_trait_distribution <- function(nodes, f.qk.yi, model, pi.k)
{
    sigma <- 1
    if (model=="1PL"){
        pi.k <- colMeans(f.qk.yi)
        sigma <- sqrt(sum(nodes^2*pi.k))
        pi.k <- tam_dnorm_discrete(x=nodes, mean=0, sd=sigma)
    }
    #-- output
    res <- list(sigma=sigma, pi.k=pi.k)
    return(res)
}
