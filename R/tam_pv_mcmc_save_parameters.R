## File Name: tam_pv_mcmc_save_parameters.R
## File Version: 0.04

tam_pv_mcmc_save_parameters <- function( beta, beta_samples,
        variance, variance_samples, deviance, deviance_samples,
        theta_samples_mean, theta_samples_sd, theta )
{
    G <- length(variance)
    #--- beta
    beta_groups <- attr(beta_samples, "beta_groups")
    index <- attr(beta_samples,"last_sample") + 1
    if ( ! beta_groups ){
        beta_samples[ index, ] <- as.vector(beta)
    }
    if ( beta_groups ){
        beta_index <- attr(beta_samples, "beta_index")
        for (gg in 1:G){
            beta_samples[ index, beta_index[[gg]] ] <- as.vector(beta[[gg]])
        }
    }
    attr(beta_samples,"last_sample") <- index

    #--- variance
    for (gg in 1:G){
        variance_gg <- variance[[gg]]
        variance_gg_index <- ( attr(variance_samples, "variance_index") )[[gg]]
        var_gg_vector <- as.vector( variance_gg[ attr(variance_samples, "variance_select") ] )
        variance_samples[ index, variance_gg_index ] <- var_gg_vector
    }
    attr(variance_samples,"last_sample") <- index

    #--- deviance
    deviance_samples[index] <- deviance
    attr(deviance_samples,"last_sample") <- index

    #--- theta parameters
    theta_samples_mean <- theta_samples_mean + theta
    theta_samples_sd <- theta_samples_sd + theta^2

    #--- OUTPUT
    res <- list( beta_samples=beta_samples, variance_samples=variance_samples,
                    deviance_samples=deviance_samples, theta_samples_mean=theta_samples_mean,
                    theta_samples_sd=theta_samples_sd)
    return(res)
}
