## File Name: tam_mml_3pl_deviance.R
## File Version: 0.07

tam_mml_3pl_deviance <- function( hwt0 , rfx , res.hwt , pweights , snodes,
        deviance=NA, deviance.history=NULL , iter=NULL )
{
    rfx <- NULL
    olddeviance <- deviance
    #---- calculate deviance
    if ( snodes == 0 ){
        rfx <- rowSums( hwt0 )
        deviance <- - 2 * sum( pweights * log( rfx ) )
    } else {
        deviance <- - 2 * sum( pweights * log( res.hwt$rfx   ) )
    }
    #----- deviance change
    rel_deviance_change <- abs( ( deviance - olddeviance ) / deviance  )
    deviance_change <- abs( ( deviance - olddeviance )  )
    #----- deviance_history
    if (!is.null(deviance.history)){
        deviance.history[iter,2] <- deviance
    }
    #----- OUTPUT
    res <- list( rfx = rfx, deviance = deviance, deviance_change=deviance_change ,
                rel_deviance_change=rel_deviance_change,
                deviance.history=deviance.history)
    return(res)
}
