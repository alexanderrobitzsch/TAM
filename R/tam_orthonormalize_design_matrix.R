## File Name: tam_orthonormalize_design_matrix.R
## File Version: 0.05

tam_orthonormalize_design_matrix <- function(x, w, eps=1e-10)
{
    cov_x <- tam_weighted_cross_product(x=x, w=w)
    dimnames(cov_x) <- NULL
    svd_cov_x <- svd(cov_x)
    Aj <- svd_cov_x$d
    Qj <- svd_cov_x$u
    ind <- which( Aj > eps )
    Qj <- Qj[,ind]
    Aj <- Aj[ind]
    # transformation matrix
    Wt <- Qj %*% diag( 1 / sqrt(Aj) )
    Wt_inv <- Qj %*% diag(sqrt(Aj))
    # orthonormalized x
    xo <- x %*% Wt
    #-- output
    res <- list(xo=xo, Wt=Wt, Wt_inv=Wt_inv)
    return(res)
}
