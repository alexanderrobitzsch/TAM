## File Name: tam_linking_function_haebara_loss.R
## File Version: 0.061


tam_linking_function_haebara_loss <- function(x, type, pow_rob_hae=1, eps=1e-4)
{
    if (type=='Hae'){
        y <- x^2
    }
    if (type=='RobHae'){
        if (pow_rob_hae>1e-16){
            y <- (x^2 + eps)^(pow_rob_hae/2)
        } else {
            y <- x^2 / ( x^2 + eps )
        }
    }
    return(y)
}
