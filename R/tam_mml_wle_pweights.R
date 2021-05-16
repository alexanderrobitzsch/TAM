## File Name: tam_mml_wle_pweights.R
## File Version: 0.01

tam_mml_wle_pweights <- function(score.resp, pweights)
{
    if (!is.null(score.resp)){
        pweights <- rep(1,nrow(score.resp))
    }
    return(pweights)
}
