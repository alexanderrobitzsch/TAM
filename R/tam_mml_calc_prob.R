## File Name: tam_mml_calc_prob.R
## File Version: 9.407


#--- calc_prob: Calculation of probabilities
tam_mml_calc_prob <- function(iIndex, A, AXsi, B, xsi, theta,
            nnodes, maxK, recalc=TRUE, use_rcpp=FALSE, maxcat=NULL,
            avoid_outer=FALSE)
{
    if (use_rcpp){
        if ( is.null(maxcat) ){ use_rcpp <- FALSE }
        if ( ! recalc ){ use_rcpp <- FALSE }
    }
    if ( ! use_rcpp ){
        #-- R function
        res <- tam_mml_calc_prob_R( iIndex=iIndex, A=A, AXsi=AXsi, B=B, xsi=xsi,
                    theta=theta, nnodes=nnodes, maxK=maxK, recalc=recalc,
                    avoid_outer=avoid_outer)
        rprobs <- res$rprobs
        AXsi <- res$AXsi
    } else {
        #-- Rcpp function
        res <- tam_rcpp_calc_prob( A=as.vector(A), dimA=dim(A), xsi=xsi,
                        maxcat=maxcat, AXsi0=AXsi, iIndex=iIndex, theta=theta, B=as.vector(B) )
        LI <- length(iIndex)
        rprobs <- array( res$rprobs, dim=c(LI,maxK,nnodes) )
        AXsi <- res$AXsi
    }
    #---- output
    res <- list("rprobs"=rprobs, "AXsi"=AXsi)
    return(res)
}

calc_prob.v5 <- tam_mml_calc_prob
tam_calc_prob <- tam_mml_calc_prob
