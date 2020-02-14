## File Name: plot_tam_grouped_wle.R
## File Version: 0.123


plot_tam_grouped_wle <- function( tamobj, tammodel, wle, ngroups, resp)
{
    if (is.null(wle)){
        if (tammodel=="mml") {
            wleobj <- tam.wle(tamobj)
            wle <- wleobj$theta
        } else {
            wle <- tamobj$WLE    # model is jml
        }
    }
    q1 <- 1 / ngroups
    quant <- unique( stats::quantile(wle, probs=seq(q1, 1 - q1, by=q1), na.rm=TRUE ) )
    wle_intervals <- c( -Inf, quant, Inf)
    groupnumber <- as.numeric( cut( wle, breaks=wle_intervals ) )
    aggr <- stats::aggregate(wle, list(groupnumber), mean, na.rm=TRUE)
    theta2 <- aggr$x
    ngroups <- length(theta2)
    d <- data.frame(wle, resp)
    d1 <- d
    d2 <- d1[-1]
    #--- output
    res <- list(wle=wle, theta2=theta2, d=d, d1=d1, d2=d2,
                    groupnumber=groupnumber, ngroups=ngroups,
                    wle_intervals=wle_intervals)
    return(res)
}
