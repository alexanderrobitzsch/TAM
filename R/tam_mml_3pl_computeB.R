## File Name: tam_mml_3pl_computeB.R
## File Version: 0.07


# function for computation of item loadings
tam_mml_3pl_computeB <- function( Edes, gammaslope, E )
{
    B <- tam_rcpp_mml_3pl_compute_B( Edes=Edes, gammaslope=gammaslope,
                    dimE=dim(E) )$B
    B <- array( B, dim(E)[1:3] )
    return(B)
}
