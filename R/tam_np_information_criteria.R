## File Name: tam_np_information_criteria.R
## File Version: 0.08

tam_np_information_criteria <- function(dev, n_est, n)
{
    ic <- list(deviance=dev, loglike=-dev/2, np=n_est, Npars=n_est,    n=n)
    ic <- tam_mml_ic_criteria(ic=ic)
    return(ic)
}
