## File Name: tam_normalize_vector.R
## File Version: 0.02

tam_normalize_vector <- function(x)
{
    x <- x / sum(x)
    return(x)
}
