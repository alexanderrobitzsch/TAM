## File Name: tam_ghp_number_informations.R
## File Version: 0.02

tam_ghp_number_informations <- function(pweights, resp.ind)
{
    ghp_obs <- NULL
    if (!is.null(resp.ind)){
        ghp_obs <- sum(pweights*resp.ind)
    }
    return(ghp_obs)
}
