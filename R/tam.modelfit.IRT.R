## File Name: tam.modelfit.IRT.R
## File Version: 0.03


###############################################################
tam.modelfit.IRT <- function( object, progress=TRUE )
{
    resp <- IRT.data(object)
    probs <- IRT.irfprob(object)
    theta <- attr( probs, "theta" )
    post <- IRT.posterior( object )
    res <- tam.modelfit.args( resp, probs, theta, post, progress)
    return(res)
}
#############################################################
