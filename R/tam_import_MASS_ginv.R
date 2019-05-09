## File Name: tam_import_MASS_ginv.R
## File Version: 0.02

tam_import_MASS_ginv <- function(X, ...)
{
    require_namespace_msg("MASS")
    y <- MASS::ginv(X=X, ...)
    return(y)
}
