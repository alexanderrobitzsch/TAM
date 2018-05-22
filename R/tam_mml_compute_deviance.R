## File Name: tam_mml_compute_deviance.R
## File Version: 0.15

tam_mml_compute_deviance <- function( loglike_num, loglike_sto, snodes,
    thetawidth, pweights, deviance=NA, deviance.history=NULL, iter=NULL,
    logprior_xsi=NULL )
{
    olddeviance <- deviance

    deviance <- 0
    penalty_xsi <- 0

    if ( ! is.null(logprior_xsi) ){
        penalty_xsi <- - 2 * sum( logprior_xsi )
        deviance <- deviance + penalty_xsi
    }
    # calculate deviance
    if ( snodes==0 ){
        deviance <- deviance - 2 * sum( pweights * log( loglike_num * thetawidth ) )
    } else {
        deviance <- deviance - 2 * sum( pweights * log( loglike_sto ) )
    }
    #----- deviance change
    rel_deviance_change <- abs( ( deviance - olddeviance ) / deviance  )
    deviance_change <- abs( ( deviance - olddeviance )  )
    deviance_change_signed <- deviance - olddeviance

    #----- deviance_history
    if ( ! is.null(deviance.history) ){
        deviance.history[iter,2] <- deviance
    }

    #----- OUTPUT
    res <- list( deviance=deviance, deviance_change=deviance_change,
                rel_deviance_change=rel_deviance_change,
                deviance.history=deviance.history, penalty_xsi=penalty_xsi,
                deviance_change_signed=deviance_change_signed)
    return(res)
}

