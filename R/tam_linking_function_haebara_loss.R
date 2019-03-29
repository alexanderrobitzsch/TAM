## File Name: tam_linking_function_haebara_loss.R
## File Version: 0.03


tam_linking_function_haebara_loss <- function(x, type, pow_rob_hae=1, eps=1e-4)
{
    if (type=="Hae"){
        y <- x^2
    }
    if (type=="RobHae"){
        y <- (x^2 + eps)^(pow_rob_hae/2)
    }
    return(y)
}
