## File Name: tam.modelfit.args.R
## File Version: 0.08


#-- tam.modelfit with user defined input
tam.modelfit.args <- function( resp, probs, theta, post, progress=TRUE )
{
    resp.ind <- as.matrix( 1- is.na(resp) )
    tamobj <- list( resp=resp, rprobs=probs, theta=theta, hwt=post,
                    resp.ind=resp.ind )
    res <- tam.modelfit( tamobj=tamobj, progress=progress)
    return(res)
}

