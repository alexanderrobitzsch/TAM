## File Name: tam_calc_counts.R
## File Version: 9.13

#######################################################
# calculate counts
tam_calc_counts <- function( resp, theta, resp.ind,
    group, maxK, pweights, hwt )
{
    TP <- nrow(theta)
    I <- ncol(resp)
    if ( is.null( group )){
        group <- rep(1, nrow(resp))
    }
    G <- length( unique( group ))
    # n.ik [ 1:TP, 1:I, 1:(K+1), 1:G ]
    n.ik <- array( 0, dim=c(TP,I,maxK, G ))
    for (gg in 1:G){    # gg <- 1
        ind.gg <- which( group==gg )
        for (kk in 1:(maxK)){
            dkk2 <- ( resp[ ind.gg, ]==(kk-1) ) * resp.ind[ ind.gg, ] * pweights[ind.gg]
            # t( t(A) * B )=t(B) * A=crossprod(B,A)
            n.ik[,,kk,gg] <- crossprod( hwt[ind.gg,], dkk2 )
        }
    }

    # calculate pi.k
    pi.k <- matrix( pweights, nrow=nrow(resp), ncol=ncol(hwt) )
    prob.theta <- matrix( NA, nrow=TP, ncol=G)
    for (gg in 1:G){
        # gg <- 1
        ind <- which( group==gg )
        pi.k1 <- colSums( pi.k[ind,] * hwt[ind,] ) / colSums( pi.k[ind,] )
        prob.theta[,gg] <- pi.k1 / sum( pi.k1 )
    }
    res <- list( "n.ik"=n.ik, "pi.k"=prob.theta)
    return(res)
}
#####################################

tam.calc.counts <- tam_calc_counts
.tam.calc.counts <- tam_calc_counts
