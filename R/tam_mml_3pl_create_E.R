## File Name: tam_mml_3pl_create_E.R
## File Version: 0.06


####################################################
# create E matrix
tam_mml_3pl_create_E <- function( resp, E, Q, gammaslope.des,
        Q.fixed=NULL )
{
    Qdes <- NULL
    gammaslope.fixed <- NULL
    if ( is.null(E) ){
        maxKi <- apply( resp, 2, max, na.rm=TRUE )
        I <- ncol(resp)
        if ( is.null(Q) ){ Q <- matrix( 1, nrow=I, ncol=1 ) }
        D <- ncol(Q)
        maxK <- max( maxKi ) + 1
        if ( gammaslope.des=="2PL" ){
            Ngam <- sum( abs(Q) > 0 )
        }
        if ( gammaslope.des=="2PLcat" ){
            Ngam <- sum( rowSums(( abs(Q) > 0 )  )*maxKi )
        }
        ng <- 1
        kk <- 1
        vv <- 1
        Qdes <- matrix( 0, nrow=maxK*I*D, ncol=5 )
        colnames(Qdes) <- c("gammapar", "item", "dim", "category", "Qval")
        for (ii in 1:I){
            for (dd in 1:D){
                if ( Q[ii,dd] !=0 ){
                    for (kk in 1:maxKi[ii]){
                        Qdes[vv,1] <- ng
                        Qdes[vv,2:3] <- c(ii,dd)
                        Qdes[vv,4] <- kk
                        if ( gammaslope.des=="2PL" ){
                            Qdes[vv,5] <- Q[ii,dd]*kk
                        }
                        if ( gammaslope.des=="2PLcat" ){
                            Qdes[vv,5] <- Q[ii,dd]
                        }
                        vv <- vv + 1
                        if ( ( kk==maxKi[ii] ) & ( gammaslope.des=="2PL") ){
                            ng <- ng + 1
                        }
                        if (  gammaslope.des=="2PLcat" ){
                            ng <- ng + 1
                        }
                    }
                }
            }  # end dd
        }  # end ii
        Qdes <- as.data.frame( Qdes[ 1:(vv-1), ] )
        Ngam <- max( Qdes$gammapar )
        gammaslope.fixed <- NULL
        # fixed gammaslope parameters
        Qdes$gamma.fixed <- NA
        if ( ! is.null(Q.fixed) ){
            for (dd in 1:D){
                # dd <- 1
                Q1 <- Q.fixed[, dd ]
                ind.dd <- which( ! is.na( Q1) )
                if ( length(ind.dd) > 0 ){
                    I1 <- length(ind.dd)
                    for (ii in 1:I1){
                        i2 <- which( ( Qdes$item==ind.dd[ii] ) &
                                  (    Qdes$dim==dd )    )
                        Qdes[i2,"gamma.fixed"] <- Q1[ ind.dd[ii] ]
                    }
                }  # end if len(ind.dd) > 0
            } # end dd
            gam1 <- stats::aggregate( Qdes$gamma.fixed, list(Qdes$gammapar), mean )
            gam1 <- stats::na.omit(gam1)
            gammaslope.fixed <- gam1[, c(1,2) ]
            colnames(gammaslope.fixed) <- NULL
        }  # end ! is.null(Q.fixed)

        #****
        E <- array( 0, dim=c(I,maxK, D, Ngam ) )
        for (ee in 1:(nrow(Qdes)) ){
            # ee <- 1
            Qdes.ii <- Qdes[ee,]
            E[ Qdes.ii$item, Qdes.ii$category + 1, Qdes.ii$dim, Qdes.ii$gammapar ] <- Qdes.ii$Qval
        }
    }
    #--- OUTPUT
    res <- list(E=E, Qdes=Qdes, gammaslope.fixed=gammaslope.fixed )
    return(res)
}
####################################################################

.mml.3pl.create.E <- tam_mml_3pl_create_E
