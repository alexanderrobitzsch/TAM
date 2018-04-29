## File Name: tam_deviance_history_init.R
## File Version: 0.02

tam_deviance_history_init <- function(maxiter)
{
    deviance.history <- matrix( 0 , nrow=maxiter , ncol = 2)
    colnames(deviance.history) <- c("iter" , "deviance")
    deviance.history[,1] <- 1:maxiter
    return(deviance.history)
}
