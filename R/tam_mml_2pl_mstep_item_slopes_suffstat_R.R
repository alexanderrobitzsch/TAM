## File Name: tam_mml_2pl_mstep_item_slopes_suffstat_R.R
## File Version: 0.06


tam_mml_2pl_mstep_item_slopes_suffstat_R <- function( rprobs, maxK, LIT, TP, itemwt, theta, dd,
            items.temp, items.conv, xbar, xbar2, xxf, xtemp, irtmodel )
{
    for ( k in 1:maxK ){
        rprobs.k <- matrix( rprobs[,k,], nrow=LIT, ncol=TP )
        rpr.it <- t( rprobs.k ) * itemwt[,items.temp]
        ttheta.dd2 <- t( theta[,dd,drop=FALSE]^2)
        ttheta.dd <- t( theta[,dd,drop=FALSE] )
        xbar[items.temp,k] <- ttheta.dd %*% rpr.it
        xxf[items.temp,k] <- ttheta.dd2 %*% rpr.it
        if ( irtmodel=="2PL" ){     # begin usual 2PL
            xbar2[items.temp,k] <- ttheta.dd2 %*% ( t( rprobs.k^2 ) * itemwt[,items.temp] )
        }
        if ( irtmodel %in% c("GPCM","GPCM.design") ){     # begin GPCM
            xxf[items.temp,k] <- xxf[items.temp,k] * (k-1)^2
            xtemp <- xtemp + tam_matrix2(theta[,dd],nrow=LIT,ncol=TP) * rprobs[,k,] * ( k-1 )
        }
        if ( irtmodel=="2PL.groups"){     # begin 2PL groups
            temp1 <- tcrossprod( ttheta.dd2, rprobs[,k,]^2*t(itemwt[,items.temp]) )
            xbar2[items.temp,k] <- temp1
        }
        if ( ! is.null( items.conv) ){
            xbar[ items.conv, ] <- 0
            xxf[ items.conv, ] <- 0
            xbar2[ items.conv, ] <- 0
        }
    }    # end categories k
    #--- output
    res <- list( xbar=xbar, xbar2=xbar2, xxf=xxf, xtemp=xtemp)
    return(res)
}
