## File Name: tam_irt_parameterization.R
## File Version: 0.13


tam_irt_parameterization <- function(resp, maxK, B, AXsi, irtmodel="2PL",
    tam_function="tam.mml", skillspace="normal")
{
    items <- colnames(resp)
    I <- length(items)
    item_irt <- NULL
    is_poly <- maxK > 2
    do_irt_param <- TRUE
    if (dim(B)[3] > 1 ){
        do_irt_param <- FALSE
    }
    if (irtmodel=="tam.mml.2pl"){
        if (tam_function=="2PL"){
            do_irt_param <- FALSE
        }
    }
    if (irtmodel=="tam.mml.3pl"){
        if (skillspace!="normal"){
            do_irt_param <- FALSE
        }
    }
    #* do IRT parameterization
    if (do_irt_param){
        nc <- maxK+1
        if (! is_poly){
            nc <- 2
        }
        AXsi <- - AXsi
        item_irt <- matrix(NA, nrow=I, ncol=nc)
        lab <- NULL
        if (is_poly){
            lab <- paste0("tau.Cat",1:(maxK-1) )
        }
        colnames(item_irt) <- c("alpha", "beta", lab)
        item_irt <- data.frame( item=items, item_irt)
        alpha <- B[,2,1]
        item_irt$alpha <- alpha
        xsi_irt <- AXsi / alpha
        for (ii in 1:I){
            xsi_irt_ii <- xsi_irt[ii,]
            Ki <- sum( 1-is.na(xsi_irt_ii) ) - 1
            beta_est <- xsi_irt_ii[Ki+1] / Ki
            item_irt[ii,"beta"] <- beta_est
            if (Ki>1){
                for (kk in 1:Ki){
                    item_irt[ii,paste0("tau.Cat",kk)] <- xsi_irt_ii[kk+1] - beta_est - xsi_irt_ii[kk]
                }
            }
        }
    }
    #--- output
    return(item_irt)
}
