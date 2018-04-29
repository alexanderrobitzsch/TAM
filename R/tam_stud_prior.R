## File Name: tam_stud_prior.R
## File Version: 9.22


#############################################################
#############################################################
tam_stud_prior <-  function(theta , Y , beta , variance , nstud ,
            nnodes , ndim , YSD , unidim_simplify , snodes=0 ,
            normalize = FALSE )
{
    # a0 <- Sys.time()
    ##################################
    # SINGLE DIMENSION
    ##################################
    if(ndim == 1) {
        if ( ! unidim_simplify  ){
            theta_rep <- rep(theta, each = nstud)
            M_gwt <- Y %*% beta
            gwt <- matrix( stats::dnorm( theta_rep, mean=M_gwt, sd = sqrt(variance)),
                        nrow = nstud)
        } else {
            TP <- nrow(theta)
            M_gwt <- as.numeric( Y[1,] %*% beta )
            gwt <- matrix( stats::dnorm(theta[,1] , mean=M_gwt , sd = sqrt(variance[1,1])),
                                nrow = nstud , ncol=TP , byrow=TRUE)
        }
    }

    ##################################
    # MULTIPLE DIMENSIONS
    ##################################
    if(ndim > 1){
        mu <- Y %*% beta     #mean vector for each student: dimensions nstud by ndim
        eps <- 1E-7

        # Stabilization of the covariance matrix
        variance <- tam_ginv(x = variance, eps=.05)
        # compute density
        varInverse <- solve(variance)
        detvar <- det(variance)
        coeff <- 1/sqrt( (2*pi)^ndim * detvar )
        gwt <- matrix( 0 , nrow=nstud , ncol=nnodes )

        #*** SD(Y) = 0
        if ( YSD ){
            gwt <- tam_rcpp_prior_normal_density_unequal_means( theta=theta, mu=mu ,
                     varInverse=varInverse, COEFF=coeff)
        }
        if ( ! YSD){
            gwt <- tam_rcpp_prior_normal_density_equal_means( theta=theta, mu=mu,
                           varInverse=varInverse, COEFF=coeff)
            gwt <- matrix( gwt , nrow=nstud , ncol=nnodes , byrow=TRUE)
        }
    }
    #---- normalization
    if (normalize){
        gwt <- tam_normalize_matrix_rows(gwt)
    }
    #--- output
    return(gwt)
}
#####################################################################

stud_prior.v2 <- tam_stud_prior


#  cat(" * prior Ysd") ; a1 <- Sys.time(); print(a1-a0) ; a0 <- a1
