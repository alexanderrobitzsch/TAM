## File Name: tam_np_create_spline_basis_hermite.R
## File Version: 0.11


tam_np_create_spline_basis_hermite <- function(nodes, n_basis)
{
    TP <- length(nodes)
    desm <- matrix(1, nrow=TP, ncol=n_basis+2)
    x <- nodes
    desm[,2] <- x
    if (n_basis>0){
        for (nn in 2L:(n_basis+1) ){
            # apply recursive rule for computing Hermite polynomials
            desm[,nn+1] <- x*desm[,nn] - (nn-1)*desm[,nn-1]
        }
    }
    return(desm)
}
