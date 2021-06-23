## File Name: tam.cb.R
## File Version: 0.16



tam.cb <- function( dat, wlescore=NULL, group=NULL, max_ncat=30, progress=TRUE,
            pweights=NULL, digits_freq=5)
{
    resp <- dat
    wgt <- pweights
    if (is.null(wgt)){
        wgt <- rep(1,nrow(resp))
    }
    I <- ncol(resp)
    if ( is.null(wlescore) ){
        est_wle <- 0
        wlescore <- rep(1, nrow(resp) )
    } else {
        est_wle <- 1
    }

    resp0 <- resp
    wlescore0 <- wlescore

    # define progress bar
    if ( is.null(group) ){
        group <- rep(1, nrow(resp) )
    }
    groups <- sort( unique( group )    )
    G <- length(groups)
    I <- ncol(resp)
    dfr <- NULL
    for (gg in 1:G){
        ind.gg <- which( group==groups[gg] )
        resp <- resp0[ ind.gg, ]
        wlescore <- wlescore0[ ind.gg ]
        wgt1 <- wgt[ind.gg]
        prg <- round( seq( 1, I, len=10 ) )
        prg[ prg==I ] <- I-1
        if (progress){
            cat("|")
            cat( paste( rep("*", 10 ), collapse="") )
            cat("| Group", groups[gg], "\n|")
            prg <- round( seq( 1, I, len=10 ) )
            prg[ prg==I ] <- I-1
        }

        if ( ! progress ){
            prg <- 1
        }

        dfr.gg <- data.frame( "group"=groups[gg], "groupindex"=gg,
                    "itemno"=1:I, "item"=colnames(resp0))
        nar <- is.na(resp)
        dfr.gg$N <- colSums(1-nar)
        dfr.gg$W <- colSums(wgt1*(1-nar))
        dfr.gg$miss_prop <- colSums(wgt1*nar)/sum(wgt1)
        dfr.gg$is_numeric <- 0
        dfr.gg$M <- NA
        dfr.gg$kurtosis <- dfr.gg$skewness <- dfr.gg$SD <- NA
        dfr.gg$Min <- NA
        dfr.gg$Max <- NA
        dfr.gg$N_unique_val <- NA
        dfr.gg$freq <- ""
        if (est_wle){
            dfr.gg$r.WLE <- NA
        }

        resp1 <- data.matrix(frame=resp)
        for (ii in 1:I){
            v1 <- resp[,ii]
            v2 <- paste(v1)
            v3 <- suppressWarnings(as.numeric(v2))
            is_num <- FALSE
            if ( mean(is.na(v3))<1 ){
                v2 <- v3
                is_num <- TRUE
                dfr.gg$is_numeric[ii] <- 1
            }
            if ( is_num ){
                dfr.gg$M[ii] <- weighted_mean(x=v3, w=wgt1)
                dfr.gg$SD[ii] <- weighted_sd(x=v3, w=wgt1)
                dfr.gg$skewness[ii] <- weighted_skewness(x=v3, w=wgt1)
                dfr.gg$kurtosis[ii] <- weighted_kurtosis(x=v3, w=wgt1)
                dfr.gg$Min[ii] <- min(v2, na.rm=TRUE)
                dfr.gg$Max[ii] <- max(v2, na.rm=TRUE)
                if (est_wle){
                    x2 <- data.frame( v3, wlescore )
                    ind <- which(rowSums(is.na(x2))==0)
                    c1 <- stats::cov.wt(x2[ind,], wt=wgt1[ind], method="ML")$cov
                    eps <- 1e-15
                    diag(c1) <- diag(c1) + eps
                    dfr.gg$r.WLE[ii] <- stats::cov2cor(c1)[1,2]
                }
            }
            l1 <- length(setdiff(unique(v1),NA))
            dfr.gg$N_unique_val[ii] <- l1
            if (l1 < max_ncat){
                wt <- weighted_table(v2, w=wgt1)
                wt <- wt / sum(wt)
                dfr.gg$freq[ii] <- paste0( " ", paste0( paste0( names(wt), " : ", round(wt,digits_freq)),
                                        collapse=" # " ) )
            }
            if ( ii %in% prg){
                cat("-")
                utils::flush.console()
            }

        }
        dfr <- rbind( dfr, dfr.gg )
        if (progress){
            cat("|\n")
        }
    } # end group
    dfr <- dfr[ order( paste0( 10000+ dfr$itemno, dfr$group ) ), ]
    dfr <- data.frame( "index"=seq(1,nrow(dfr) ), dfr )
    return(dfr)
}

