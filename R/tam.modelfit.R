## File Name: tam.modelfit.R
## File Version: 9.406


# Q3 statistic and model fit statistics for objects of class tam
tam.modelfit <- function( tamobj, progress=TRUE )
{
    s1 <- Sys.time()
    #--- extract data from tam object
    mod1 <- tamobj
    resp0 <- resp <- mod1$resp
    resp <- as.matrix(resp)
    jkunits <- 1
    maxKi <- apply( resp, 2, max, na.rm=TRUE)
    resp.ind <- mod1$resp.ind
    resp_ind <- resp_ind_bool <- ! is.na(resp)
    rprobs <- mod1$rprobs
    theta <- mod1$theta
    hwt <-  mod1$hwt
    maxK <- dim(rprobs)[2]
    TP <- dim(rprobs)[3]
    residM <- matrix( 0, nrow=nrow(resp), ncol=ncol(resp) )
    N <- nrow(resp)
    I <- ncol(resp)

    #--- calculate Q3 and residuals
    if (progress){
        cat("**** Calculate Residuals \n")
        utils::flush.console()
    }
    RR <- I*(I-1) / 2
    #-- compute residuals
    residM <- tam_rcpp_modelfit_residuals( rprobs=as.vector(rprobs),
                    resp=resp, I=I, TP=TP, maxK=maxK, maxKi=maxKi, hwt=hwt,
                    resp_bool=resp_ind_bool )
    resp[ resp.ind==0 ] <- NA
    residM <- resp - residM

    #-- compute Q3 statistic
    dfr <- tam_rcpp_modelfit_q3( residM=residM, resp_bool=resp_ind_bool )
    dfr <- as.data.frame(dfr)
    colnames(dfr) <- c("index1", "index2", "Q3", "aQ3" )
    dfr$aQ3 <- dfr$Q3 - mean(dfr$Q3, na.rm=TRUE)
    dfr2 <- data.frame( "index1"=dfr$index1, "index2"=dfr$index2 )
    dfr2$Q3 <- dfr$Q3
    dfr2$aQ3 <- dfr$aQ3

    #-- compute p value
    N <- nrow(resp)
    se1 <- - abs( tam_fisherz(dfr2$aQ3) * sqrt( N -3 ) )
    dfr2$p <- 2 * stats::pnorm( se1  )
    dfr <- dfr2
    dfr <- dfr[ order( dfr$aQ3, decreasing=TRUE), ]
    dfr$p.holm <- stats::p.adjust( dfr$p, method="holm")

    #-- include sample size of each item pair
    cp1 <- crossprod( resp_ind )
    dfr$N_itempair <- cp1[ as.matrix(dfr[, c("index1", "index2" ) ]) ]
    cn <- colnames(resp)

    #**** fit statistic
    stat.MADaQ3 <- data.frame( "MADaQ3"=mean( abs(dfr$aQ3), na.rm=TRUE ) )

    #**** order item pair table
    dfr <- data.frame( "item1"=cn[ dfr$index1 ], "item2"=cn[dfr$index2], dfr )
    dfr <- dfr[ order(dfr$p), ]
    stat.MADaQ3$maxaQ3 <- abs(dfr$aQ3[1])
    stat.MADaQ3$p <- dfr$p.holm[1]

    #***** calculate observed and expected counts
    if (progress){
        cat("**** Calculate Counts \n")
        utils::flush.console()
    }
    res1 <- tam_rcpp_modelfit_counts( resp0=as.matrix(resp0),
                resp_bool=resp_ind_bool, rprobs=as.vector(rprobs), hwt=as.matrix(hwt),
                maxKi=maxKi, maxK=maxK )
    obs_counts <- res1$obs_counts
    exp_counts <- res1$exp_counts
    pair_exists <- rowSums(obs_counts) > 0

    chi2.stat <- data.frame(res1$maxKiM)
    colnames(chi2.stat) <- c("index1","index2", "maxK1", "maxK2", "df")
    eps <- 1e-10
    chi2.stat$chi2[pair_exists] <- rowSums( ( obs_counts - exp_counts )^2 / ( exp_counts + eps ) )[pair_exists]
    chi2.stat$p[pair_exists] <- 1- stats::pchisq( chi2.stat$chi2[pair_exists], df=chi2.stat$df[pair_exists]  )
    chi2.stat$p.holm[pair_exists] <- stats::p.adjust( chi2.stat$p[pair_exists], method="holm")

    #--- calculate covariance and correlation
    scorematrix <- cbind( rep( 0:(maxK-1), maxK ),rep(0:(maxK-1), each=maxK) )
    #--- observation covariances and correlations
    if (progress){
        cat("**** Calculate Covariances \n")
        utils::flush.console()
    }


    #--- conditional covariance
    res2 <- tam_rcpp_modelfit_ccov( counts=obs_counts, scorematrix=scorematrix, adjust=1 )
    res2e <- tam_rcpp_modelfit_ccov( counts=exp_counts, scorematrix=scorematrix, adjust=0 )

    #--- compute fit statistics
    residcov <- 100*mean( abs( res2$cov_ij - res2e$cov_ij ), na.rm=TRUE )
    srmr <- mean( abs( res2$cor_ij - res2e$cor_ij ), na.rm=TRUE  )
    srmsr <- sqrt( mean( ( res2$cor_ij - res2e$cor_ij )^2, na.rm=TRUE  ) )
    fitstat <- c( residcov, srmr, srmsr )
    names(fitstat) <- c("100*MADCOV", "SRMR","SRMSR")

    # create matrix of Q3 and adjusted Q3 statistics
    dfr1 <- dfr
    dfr1 <- dfr1[ order( 10000*dfr1$index1 + dfr1$index2 ), ]
    Q3.matr <- matrix(NA, I, I )
    rownames(Q3.matr) <- colnames(Q3.matr) <- colnames(resp)
    aQ3.matr <-  Q3.matr
    RR <- nrow(dfr1)
    for (rr in 1:RR){
        ii1 <- dfr1$index1[rr]
        ii2 <- dfr1$index2[rr]
        Q3.matr[ ii1, ii2 ] <- Q3.matr[ii2,ii1] <- dfr1$Q3[rr]
        aQ3.matr[ ii1, ii2 ] <- aQ3.matr[ii2,ii1] <- dfr1$aQ3[rr]
    }
    # inclusion of item fit statistics
    chisquare.itemfit <- data.frame("item"=colnames(resp), "index"=1:I )
    for (ii in 1:I){
        h1 <- dfr[ dfr$index1==ii | dfr$index2==ii, ]
        h1 <- h1[!is.nan(h1$p),]
        chisquare.itemfit$p[ii] <- min( stats::p.adjust( h1$p, method="holm") )
    }
    chisquare.itemfit$p.holm <- stats::p.adjust( chisquare.itemfit$p, method="holm")

    # maximum chi square
    modelfit.test <- data.frame( maxX2=max( chi2.stat$chi2),
                            Npairs=nrow(chi2.stat),
                            p.holm=min( chi2.stat$p.holm[pair_exists] )    )

    #** modelfit.stat
    modelfit.stat <- fitstat
    modelfit.stat["MADaQ3"] <- stat.MADaQ3["MADaQ3"]
    modelfit.stat["pmaxX2"] <- modelfit.test["p.holm"]
    modelfit.stat <- as.data.frame(modelfit.stat)

    #***** calculate summary of Q3 statistics
    Q3_summary <- data.frame( "type"=c("Q3", "aQ3" ) )
    diag(cp1) <- NA
    cp1_sum <- sum( cp1, na.rm=TRUE )
    Q3_summary[1,"M"] <- sum( Q3.matr * cp1, na.rm=TRUE ) / cp1_sum
    Q3_summary[2,"M"] <- sum( aQ3.matr * cp1, na.rm=TRUE ) / cp1_sum
    Q3_summary[1,"SD"] <- sum( Q3.matr^2 * cp1, na.rm=TRUE ) / cp1_sum
    Q3_summary[2,"SD"] <- sum( aQ3.matr^2 * cp1, na.rm=TRUE ) / cp1_sum
    Q3_summary$SD <- sqrt( Q3_summary$SD - Q3_summary$M^2 )
    Q3_summary[1,"min"] <- min( Q3.matr, na.rm=TRUE )
    Q3_summary[1,"max"] <- max( Q3.matr, na.rm=TRUE )
    Q3_summary[2,"min"] <- min( aQ3.matr, na.rm=TRUE )
    Q3_summary[2,"max"] <- max( aQ3.matr, na.rm=TRUE )
    cp10 <- 1 - is.na(cp1)
    cp10_sum <- sum( cp10, na.rm=TRUE )
    Q3_summary[1,"SGDDM"] <- sum( abs(Q3.matr) * cp10, na.rm=TRUE ) / cp10_sum
    Q3_summary[2,"SGDDM"] <- sum( abs(aQ3.matr) * cp10, na.rm=TRUE ) / cp10_sum
    Q3_summary[1,"wSGDDM"] <- sum( abs(Q3.matr) * cp1, na.rm=TRUE ) / cp1_sum
    Q3_summary[2,"wSGDDM"] <- sum( abs(aQ3.matr) * cp1, na.rm=TRUE ) / cp1_sum

    s2 <- Sys.time()
    #******* OUTPUT *******
    res <- list( stat.MADaQ3=stat.MADaQ3, chi2.stat=chi2.stat, fitstat=fitstat,
                modelfit.test=modelfit.test, stat.itempair=dfr,
                chisquare.itemfit=chisquare.itemfit, residuals=residM, Q3.matr=Q3.matr,
                aQ3.matr=aQ3.matr, Q3_summary=Q3_summary, N_itempair=cp1,
                statlist=modelfit.stat, time_diff=s2-s1 )
    class(res) <- "tam.modelfit"
    return(res)
}
################################################

# # cat("calc residM Rcpp") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1
