## File Name: tam_dmvnorm.R
## File Version: 9.16

##########################################################################
tam_dmvnorm <- function( x, mean, sigma, log=FALSE )
{
    # copied and slightly extended from the mvtnorm::dmvnorm function
    mu <- mean
    dec <- chol(sigma)
    muM <- mu
    if (is.vector(x)){
        x <- matrix( x, nrow=1)
    }
    if (( is.vector(mu) )){
        n <- nrow(x)
        d <- ncol(x)
        muM <- matrix( mu, nrow=n, ncol=d, byrow=TRUE)
    }
    D <- ncol(muM)
    tmp <- backsolve(dec, t(x - muM ), transpose=TRUE)
    rss <- colSums(tmp^2)
    logretval <- -sum(log(diag(dec))) - 0.5 * D * log(2 * pi) - 0.5 * rss
    if (! log){    logretval <- exp(logretval )}
    return(logretval)
}
##########################################################################

dmvnorm_TAM <- tam_dmvnorm
