## File Name: tam_pv_mcmc_likelihood.R
## File Version: 0.14

tam_pv_mcmc_likelihood <- function( probs, resp, resp_ind_bool, nstud, nitems , maxK )
{
    probs00 <- matrix( probs , nrow=nstud , ncol=nitems*maxK)
    loglike <- tam_rcpp_pv_mcmc_likelihood( probs=probs00, resp=resp,
                    resp_ind_bool=resp_ind_bool, maxK=maxK, nstud=nstud,
                    nitems=nitems )
    return(loglike)
}
