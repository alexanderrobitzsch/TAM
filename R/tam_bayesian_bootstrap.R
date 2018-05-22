## File Name: tam_bayesian_bootstrap.R
## File Version: 0.06

tam_bayesian_bootstrap <- function(N, sample_integers=FALSE, do_boot=TRUE)
{
    if (do_boot){
        if ( ! sample_integers){
            y <- stats::runif(N-1, min=0, max=1)
            y <- N * c(0, sort(y), 1 )
            v <- diff(y)
        } else {
            vec <- 1:N
            y <- sample(x=vec, size=N, replace=TRUE)
            v <- rep(0,N)
            names(v) <- vec
            ty <- table(y)
            v[ names(ty) ] <- ty
        }
    } else {
        v <- rep(1,N)
    }
    return(v)
}
