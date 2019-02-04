## File Name: tam_dnorm_discrete.R
## File Version: 0.01

tam_dnorm_discrete <- function(x, mean=0, sd=1)
{
    y <- stats::dnorm(x=x, mean=mean, sd=sd)
    y <- y/sum(y)
    return(y)
}
