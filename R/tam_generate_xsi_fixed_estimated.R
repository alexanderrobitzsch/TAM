## File Name: tam_generate_xsi_fixed_estimated.R
## File Version: 9.072


tam_generate_xsi_fixed_estimated <- function( xsi, A )
{
    L <- length(xsi)
    xsi.fixed.estimated <- cbind( 1:L, xsi )
    rownames(xsi.fixed.estimated) <- dimnames(A)[[3]]
    return(xsi.fixed.estimated)
}
