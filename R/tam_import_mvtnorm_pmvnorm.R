## File Name: tam_import_mvtnorm_pmvnorm.R
## File Version: 0.02

tam_import_mvtnorm_pmvnorm <- function(...)
{
    require_namespace_msg("mvtnorm")
    y <- mvtnorm::pmvnorm(...)
    return(y)
}
