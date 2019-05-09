## File Name: tam_import_mvtnorm_dmvnorm.R
## File Version: 0.01

tam_import_mvtnorm_dmvnorm <- function(...)
{
    require_namespace_msg("mvtnorm")
    y <- mvtnorm::dmvnorm(...)
    return(y)
}
