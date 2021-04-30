## File Name: tam_mml_include_NA_AXsi.R
## File Version: 0.04


tam_mml_include_NA_AXsi <- function(AXsi, maxcat=NULL, A=NULL, xsi=NULL)
{
    if (!is.null(xsi)){
        AXsi <- tam_AXsi_compute(A=A, xsi=xsi)
    }
    if (is.null(maxcat)){
        maxcat <- rep( ncol(AXsi), nrow(AXsi) )
    }
    maxK <- max(maxcat)
    include_NA <- mean(maxcat==maxK) < 1
    if ( sum( is.na(AXsi) ) > 0 ){
        include_NA <- FALSE
    }
    if( include_NA ){
        I <- length(maxcat)
        for (ii in 1:I){
            if (maxcat[ii]<maxK){
                AXsi[ii, seq(maxcat[ii]+1,maxK) ] <- NA
            }
        }
    }
    return(AXsi)
}
