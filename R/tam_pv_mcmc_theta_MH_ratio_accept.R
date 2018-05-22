## File Name: tam_pv_mcmc_theta_MH_ratio_accept.R
## File Version: 0.11

tam_pv_mcmc_theta_MH_ratio_accept <- function(loglike, log_dens_theta,
    loglike_new, log_dens_theta_new, theta_acceptance_MH, theta, theta_new)
{
    nstud <- length(log_dens_theta)
    eps <- 1E-100
    log_MH_ratio <- log( loglike_new + eps ) + log_dens_theta_new - log( loglike + eps ) - log_dens_theta
    MH_ratio <- tam_exp_overflow(x=log_MH_ratio)
    accept_MH <- ( MH_ratio > stats::runif(nstud) )
    loglike[ accept_MH ] <- loglike_new[ accept_MH ]
    theta_acceptance_MH$n_samples <- theta_acceptance_MH$n_samples + 1
    theta_acceptance_MH$accepted <- theta_acceptance_MH$accepted + accept_MH
    theta[ accept_MH, ] <- theta_new[ accept_MH, ]
    deviance <- - 2 * sum(log(loglike))
    #--- OUTPUT
    res <- list(loglike=loglike, theta=theta, deviance=deviance,
                theta_acceptance_MH=theta_acceptance_MH )
    return(res)
}
