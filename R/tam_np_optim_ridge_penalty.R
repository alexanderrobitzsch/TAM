## File Name: tam_np_optim_ridge_penalty.R
## File Version: 0.16

tam_np_optim_ridge_penalty <- function(par0, penalty_type, target_fct, lambda,
    index, deriv=0)
{
    pen <- 0
    if (penalty_type=="ridge"){
        np <- length(par0)
        ind <- seq(target_fct+1, np)
        if (deriv==0){
            pen <- lambda*sum(par0[ind]^2)
        }
        if (deriv>0){
            pen <- rep(0, np)
            ind1 <- intersect(index,ind)
        }
        if (deriv==1){
            pen[ind1] <- 2*lambda*par0[ind1]
        }
        if (deriv==2){
            pen[ind1] <- 2*lambda
        }
        if (deriv>0){
            pen <- pen[index]
        }
    }
    return(pen)
}
