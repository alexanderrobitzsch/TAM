## File Name: tam_prior_eval_log_density_one_parameter.R
## File Version: 0.06

tam_prior_eval_log_density_one_parameter <- function( density_pp, args_pp, parameter_pp, deriv=0)
{
    args_pp$x <- parameter_pp
    loc <- args_pp$mean
    scale <- args_pp$sd
    x <- args_pp$x
    if (density_pp=="norm"){
        if (deriv==0){
            res <- - 1/2*log( 2*pi ) - log( scale ) - ( x - loc  )^2 / 2 / scale^2
        }
        if (deriv==1){
            res <- - ( x -  loc ) / scale^2
        }
        if (deriv==2){
            res <- - 1 / scale^2
        }
    }
    return(res)
}
