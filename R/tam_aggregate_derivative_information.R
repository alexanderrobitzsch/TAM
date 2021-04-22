## File Name: tam_aggregate_derivative_information.R
## File Version: 0.01

tam_aggregate_derivative_information <- function(deriv, groups)
{
    res <- stats::aggregate(deriv, list(groups), sum)
    res[ res[,1]==0, 2] <- 0
    ind <- match(groups, res[,1])
    return(res[ind,2])
}
