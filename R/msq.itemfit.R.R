## File Name: msq.itemfit.R.R
## File Version: 1.06


##########################################################
# itemfit statistic in R
msq.itemfit.R <- function( dfr, FF, fitindices, fitgroups,
                    res, post1, N, TP, I, K, resp )
{

    #----------------------------------
    # loop over fit groups
    for (ff in 1:FF){
        ind.ff <- which( fitindices==ff )

        #*********
        # Outfit statistic

        #--- compute fit statistic
        fit0 <- rep(0,N)
        for (ii in ind.ff){
            stand.resid <- res$stand.resid[,,ii]
            fit1 <- rowSums( post1 * stand.resid^2, na.rm=TRUE)
            fit0 <- fit1 + fit0
        }
            fit0 <- sum(fit0) / sum( 1 - is.na( resp[,ind.ff] ) )
            dfr[ff,"Outfit"] <- fit0

        #--- compute inference

        v1 <- rep(0,N)
        for (ii in ind.ff){
            probs.ii <- res$probs.categ[,,,ii]
            kurt.ii <- array( 0, dim=c(N,TP) )
            exp.ii <- res$expected[,,ii]
            for (kk in 1:K){
                kurt.ii <- kurt.ii + probs.ii[,kk,] * ( kk - 1 - exp.ii )^4
            }
            v0 <- kurt.ii / res$variance[,,ii]^2
            v0 <- rowSums( post1 * v0, na.rm=TRUE)
            v1 <- v1 + v0
        }
        N1 <- sum( 1-is.na(resp[,ind.ff] ) )
        qi <- sum( v1 / N1^2 ) - 1/N1

        # this seems to be the adequate formula
        dfr[ff,"Outfit_t"] <- ( fit0^(1/3)-1 )* 3 / sqrt(qi) + sqrt(qi) / 3

        #***********
        # Infit statistic
        term1 <- term2 <- rep(0,N)
        for (ii in ind.ff){
            stand.resid <- res$stand.resid[,,ii]
            variance <- res$variance[,,ii]
            term1 <- term1 + rowSums( post1 * stand.resid^2 * variance, na.rm=TRUE )
            term2 <- term2 + rowSums( post1 * variance, na.rm=TRUE )
        }
        fit1 <- sum( term1, na.rm=TRUE) / sum(term2, na.rm=TRUE)
        dfr[ff,"Infit"] <- fit1

        v1 <- rep(0,N)
        for (ii in ind.ff){
            variance <- res$variance[,,ii]
            v0 <- ( kurt.ii - res$variance[,,ii]^2 )
            v0 <- rowSums( post1 * v0 )
            v1 <- v1+v0
        }
        qi <- sum(v1, na.rm=TRUE) / ( sum( term2, na.rm=TRUE) )^2
        # this seems to be a an adequate formula
        dfr[ff,"Infit_t"] <- ( fit1^(1/3)-1 )* 3 / sqrt(qi) + sqrt(qi) / 3
    }
    return(dfr)
}
#######################################################################

