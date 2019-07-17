## File Name: sirt_import_sfsmisc_QUnif.R
## File Version: 0.05

sirt_import_sfsmisc_QUnif <- function(n, min=0, max=1, n.min=1, p,
        leap=409, ...)
{
    require_namespace_msg("sfsmisc")
    r1 <- sfsmisc::QUnif(n=n, min=min, max=max, n.min=n.min, p=p,
                    leap=leap, ...)
    return(r1)
}
