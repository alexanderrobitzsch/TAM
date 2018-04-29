## File Name: tam_pv_mcmc_evaluate_likelihood.R
## File Version: 0.12

tam_pv_mcmc_evaluate_likelihood <- function(theta,
    AXsi, B, guess, resp, resp.ind, maxK, resp_ind_bool )
{
    nitems <- ncol(resp)
    nstud <- nrow(resp)
    #* compute response probabilities and likelihood
    probs <- tam_irf_3pl(theta=theta, AXsi=AXsi, B=B, guess=guess)
    like <- tam_pv_mcmc_likelihood( probs=probs, resp=resp,
                resp_ind_bool= resp_ind_bool, nstud=nstud, nitems=nitems, maxK=maxK )
    #--- OUTPUT
    return(like)
}

# z0 <- Sys.time(); active <- TRUE
# z0 <- tamcat( label = " - like", time0 = z0, active=active)
