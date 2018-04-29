## File Name: tam_Rhat_3splits.R
## File Version: 0.02

tam_Rhat_3splits <- function(parameter_samples)
{
    pss <- parameter_samples
    n <- nrow(pss)
    n0 <- floor( n/3 )
    m1 <- coda::mcmc.list( coda::mcmc( pss[ 1:n0 , ]), coda::mcmc( pss[ n0 + 1:n0, ]),
                    coda::mcmc( pss[ 2*n0 + 1:n0 , ] ) )
    gd1 <- coda::gelman.diag( m1, autoburnin=FALSE, multivariate=FALSE )
    gd1 <- gd1$psrf[,1]
    return(gd1)
}
