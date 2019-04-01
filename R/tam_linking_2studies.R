## File Name: tam_linking_2studies.R
## File Version: 0.224

tam_linking_2studies <- function( B1, AXsi1, guess1, B2, AXsi2, guess2, theta,
    wgt, type, M1=0, SD1=1, M2=0, SD2=1, fix.slope=FALSE, pow_rob_hae=1)
{
    CALL <- match.call()
    #--- preliminaries
    TP <- nrow(theta)
    K <- ncol(AXsi1)
    I <- nrow(AXsi1)
    #--- define linking function
    linking_criterion_2studies <- function(x){
        #-- study 1
        probs1 <- tam_irf_3pl(theta=theta, AXsi=AXsi1, B=B1, guess=guess1)
        probs1[ is.na(probs1) ] <- 0
        #-- study 2
        a <- x[1]
        b <- x[2]
        theta2 <- theta
        theta2[,1] <- a*theta[,1] + b
        probs2 <- tam_irf_3pl(theta=theta2, AXsi=AXsi2, B=B2, guess=guess2)
        probs2[ is.na(probs2) ] <- 0
        #-- discrepancy function
        crit <- tam_linking_irf_discrepancy(probs1=probs1, probs2=probs2, wgt=wgt,
                        type=type, pow_rob_hae=pow_rob_hae)
        return(crit)
    }
    #--- optimization
    lower <- c(-Inf,-Inf)
    upper <- c(Inf,Inf)
    if (fix.slope){
        eps <- 1E-15
        lower[1] <- 1 - eps
        upper[1] <- 1 + eps
    }
    optim_result <- stats::optim( par=c(1,0), fn=linking_criterion_2studies,
                        method="L-BFGS", lower=lower, upper=upper)
    #--- transformations
    trafo_items <- optim_result$par
    names(trafo_items) <- c("a","b")
    trafo_persons <- 1 / trafo_items
    trafo_persons["b"] <- - trafo_items["b"] / trafo_items["a"]

    #--- transformed distribution
    M_SD <- tam_linking_2studies_create_M_SD( M1=M1, SD1=SD1, M2=M2, SD2=SD2,
                    trafo_persons=trafo_persons )
    #--- transformations of item parameters
        # X=0: 0
        # X=1: B_i1 * (a*TH + b) + Axsi1_i
        # X=2: B_i2 * (a*TH + b) + Axsi2_i
        # ...
    B2_trans <- B2
    AXsi2_trans <- AXsi2
    for (kk in 2:K){
        B2_trans[,kk,] <- B2[,kk,] * trafo_items["a"]
        AXsi2_trans[,kk] <- B2[,kk,] * trafo_items["b"] + AXsi2[,kk]
    }
    #--- OUTPUT
    res <- list( optim_result=optim_result, TP=TP, I=I, M_SD=M_SD, trafo_items=trafo_items,
                    trafo_persons=trafo_persons, B1=B1, AXsi1=AXsi1, B2=B2, AXsi2=AXsi2,
                    B2_trans=B2_trans, AXsi2_trans=AXsi2_trans, guess1=guess1, guess2=guess2,
                    type=type, theta=theta,    wgt=wgt, CALL=CALL)
    class(res) <- "tam_linking_2studies"
    return(res)
}
