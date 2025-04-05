## File Name: tam_linking_joint_calc_probs.R
## File Version: 0.101

tam_linking_joint_calc_probs <- function(a, b, parameters_list_mm, theta)
{
    AXsi <- parameters_list_mm$AXsi
    B <- parameters_list_mm$B
    guess <- parameters_list_mm$guess
    theta2 <- theta
    theta2[,1] <- a*theta[,1] + b
    probs <- tam_irf_3pl(theta=theta2, AXsi=AXsi, B=B, guess=guess)
    probs[ is.na(probs) ] <- 0
    attr(probs, 'items') <- dimnames(B)[[1]]
    return(probs)
}
