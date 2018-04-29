## File Name: msq.itemfit.R
## File Version: 9.31

#######################################
# Item fit mean squares statistics
msq.itemfit <- function( object , fitindices=NULL)
{
    s1 <- Sys.time()
    CALL <- match.call()

    #--- collect necessary input
    resp <- IRT.data(object)
# a0 <- Sys.time()
    res <- predict(object)
    irf1 <- IRT.irfprob(object)
    irf1[ is.na(irf1) ] <- 0
    post1 <- IRT.posterior(object)
    NI <- dim(res$probs.categ)
    I <- NI[4]
    TP <- NI[3]
    K <- NI[2]
    N <- NI[1]
    fititem <- FALSE
    if ( is.null(fitindices) ){
        fitindices <- seq(1,I)
        fititem <- TRUE
    }

    #---- include fit statistic in Rcpp
    fitgroups <- unique( fitindices)
    FF <- length(fitgroups)
    fitindices <- match( fitindices , fitgroups )
    fitIndexM0 <- fitIndexM <- cbind( seq(1 , length(fitindices) ) , fitindices )
    m1 <- order(fitIndexM[,2] )
    fitIndexM <- as.matrix( fitIndexM[ m1 , ] - 1)
    fitIndexM0 <- as.matrix( fitIndexM0[ m1 , ] )
    fitIndexTable <- matrix( 0 , nrow=FF , ncol=3 )
    fitIndexTable[,1] <- 1:FF
    fitIndexTable[,2] <- sapply( 1:FF , FUN = function(ff){
                    g1 <- which( fitIndexM0[,2] == ff )
                    return(g1[1])
                            } )
    fitIndexTable[,3] <- sapply( 1:FF , FUN = function(ff){
                    g1 <- max( which( fitIndexM0[,2] == ff )  )
                    return(g1)
                            } )
    FIT_ <- as.matrix( fitIndexTable - 1 )

    dfr <- data.frame( "fitgroup" = fitgroups )
    if (fititem){
        dfr <- cbind( "item"= colnames(resp) , dfr )
    }

    irf1_ <- as.numeric(irf1)
    resp_bool <- ! is.na(resp)
    res0 <- tam_rcpp_msq_itemfit( resp=resp, irf1=irf1_, K=K, TP=TP, post1=post1, 
                        FIT=FIT_, fitIndexM=fitIndexM, resp_bool=resp_bool ) 
    res0 <- as.data.frame(res0)
    colnames(res0) <- c("Outfit" , "Outfit_t" , "Infit" , "Infit_t")
    dfr <- cbind( dfr , res0)

    #---   compute p values
    dfr$Outfit_p <- 2 * stats::pnorm( -abs( dfr$Outfit_t ))
    dfr$Infit_p <- 2 * stats::pnorm( -abs( dfr$Infit_t ))
    # arrange columns in data frame
    cdfr <- colnames(dfr)
    ind <- c( grep( "Outfit" , cdfr ) ,  grep( "Infit" , cdfr ) )
    ind <- c( setdiff( seq(1,ncol(dfr) ) , ind ) , ind )
    dfr <- dfr[,ind]
    # summary statistic
    vars <- c("Outfit" , "Infit")
    dfr2 <- data.frame( "fit" = vars , "M" = colMeans(dfr[,vars]) ,
                    "SD" = apply( dfr[,vars] , 2 , stats::sd ) )
    s2 <- Sys.time()
    v1 <- c(s1 , s2 )
    res <- list( "itemfit" = dfr , "summary_itemfit"=dfr2 ,    time=v1, CALL=CALL )
    class(res) <- "msq.itemfit"
    return(res)
}
######################################################

# cat("predict function\n"); a1 <- Sys.time() ; print(a1-a0); a0 <- a1
