## File Name: tam_itempartable.R
## File Version: 9.08



##################################################
# create table of item parameters
tam_itempartable <- function( resp , maxK , AXsi , B , ndim ,
            resp.ind, rprobs, n.ik, pi.k, order = FALSE ){

    if ( is.null(dimnames(B)[[1]] ) ){
        dimnames(B)[[1]] <- colnames(resp)
    }

    item1 <- data.frame( "item" = dimnames(B)[[1]] )
    item1$N <- colSums(resp.ind )
    item1$M <- colSums( resp.ind * resp , na.rm=TRUE) / colSums( resp.ind )
    maxKi <- rowSums( 1 - is.na( AXsi ) ) - 1
    I <- nrow(item1)
    item1$xsi.item <- - AXsi[ cbind(1:I , maxKi+1) ] / maxKi

    #****
    # Item fit
    # probs ... [ classes , items , categories ]
    probs <- aperm( rprobs , perm=c(3,1,2))
    pi.k <- matrix( pi.k , ncol=1 )
    b0 <- sum( B[ , 1 , ] , na.rm=TRUE )
    a0 <- 0
    if ( b0 + a0 > 0 ){
        kvec <- 0:(maxK-1)
    } else {
        kvec <- 1:(maxK-1)
    }
    for (kk in kvec){
        item1[ , paste0("AXsi_.Cat" , kk) ] <- - AXsi[,kk+1]
    }
    for (kk in kvec){
        for (dd in 1:ndim){
            item1[ , paste0("B.Cat" , kk,".Dim",dd) ] <- B[,kk+1,dd]
        }
    }
    item1 <- item1[ item1$N > 0 , ]
    # rownames(item1) <- NULL
    rownames(item1) <- paste( item1$item )

    #--- ordering rows for items
    if (order){
        item1 <- item1[ order( paste(item1$item)) , ]
    }
    #*** substitute -99 by missing
    item1[ item1 == -99 ] <- NA
    return(item1)
}


.TAM.itempartable <- tam_itempartable
