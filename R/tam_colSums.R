## File Name: tam_colSums.R
## File Version: 0.02

tam_colSums <- function(x)
{
    use_sum <- FALSE
    if (is.vector(x)){
        use_sum <- TRUE
    }
    if (is.matrix(x)){
        if (ncol(x)==1){
            use_sum <- TRUE
        }
    }
    if (use_sum){
        res <- sum(x)
    } else {
        res <- colSums(x)
    }
    return(res)
}
