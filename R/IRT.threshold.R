## File Name: IRT.threshold.R
## File Version: 9.07

###################################################
# general function for computing thresholds from a fitted
# model for which IRT.irfprob exists
IRT.threshold <- function( object , prob.lvl = .5 , type="category")
{
    irfprob <- IRT.irfprob( object )
    irfprob[ is.na(irfprob) ] <- 0
    KI <- dim(irfprob)
    thresh <- matrix( NA , nrow=KI[1] , ncol=KI[2] -1 )
    rownames(thresh) <- dimnames(irfprob)[[1]]
    colnames(thresh) <- paste0( "Cat" , seq(1 , ncol(thresh) ) )
    theta <- attr( irfprob , "theta" )
    I <- nrow(thresh)
    D <- ncol(theta)
    #*************************************
    # select item
    for (ii in 1:I){
        irf.ii <- irfprob[ ii ,,]
        # compute maximum number of categories
        rs <- rowSums( irf.ii , 1 , na.rm=TRUE )
        K <- sum( rs > 0 ) - 1
        if ( type=="item"){
            N1 <- nrow(irf.ii)
            irf1 <- irf.ii * 0:(N1-1)
            irf.ii[2,] <- colSums( irf1 ) / K
            K <- 1
        }
        vv <- 0
        for (dd in 1:D){
            a1 <- stats::aggregate( irf.ii[2,] , list(theta[,dd]) , mean )
            if ( stats::sd(a1[,2])> 1E-7 ){
                vv <- dd
            }
        }
        dd <- vv
        x1 <- theta[,dd]
        for (kk in 1:K){
            y1 <- colSums( irf.ii[seq(kk+1,K+1), , drop=FALSE] )
            thresh[ii,kk] <- tam_find_root( x1=x1, y1=y1, prob.lvl=prob.lvl, theta=theta ) 
        }
    }
    if (type=="item"){
        thresh <- as.vector(thresh[,1])
        names(thresh) <- as.vector(dimnames(irfprob)[[1]])
    }
    class(thresh) <- "IRT.threshold"
    attr(thresh , "theta") <- attr( irfprob , "theta")
    attr(thresh , "prob.theta") <- attr( irfprob , "prob.theta")
    return(thresh)
}
################################################################

###################################################
# print method for IRT.threshold
print.IRT.threshold <- function( x , ... )
{
    thresh11 <- as.matrix(x)
    attr(thresh11,"theta") <- NULL
    attr(thresh11, "prob.theta") <- NULL
    class(thresh11) <- "matrix"
    print(thresh11)
}
######################################################

