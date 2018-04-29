## File Name: tam_dtnorm.R
## File Version: 0.05

########################################
# This is a copy of msm::dtnorm
tam_dtnorm <- function(x, mean = 0, sd = 1, lower = -Inf, upper = Inf, log = FALSE)
{
    ret <- numeric(length(x))
    ret[x < lower | x > upper] <- if (log) { -Inf } else { 0 }
    ret[upper < lower] <- NaN
    ind <- x >= lower & x <= upper
    if (any(ind)) {
        denom <- stats::pnorm(upper, mean, sd) - stats::pnorm(lower, mean, sd)
        xtmp <- stats::dnorm(x, mean, sd, log)
        if (log)
            xtmp <- xtmp - log(denom)
        else xtmp <- xtmp/denom
        ret[x >= lower & x <= upper] <- xtmp[ind]
    }
    return(ret)
}
