## File Name: tam_wle_print_WLErel.R
## File Version: 0.07


#**********************************************************************
tam_wle_print_WLErel <- function( WLE.rel, ndim, digits, M_sq_error,
        WLEvar, WLEM )
{
    if (ndim==1){
        cat("\n")
        v1 <- paste0("  WLE Reliability=", round(WLE.rel,digits=digits), "\n")
        cat(v1)
        v1 <- paste0("  Average error variance=",
                        round(M_sq_error,digits=digits), "\n")
        cat(v1)
        v1 <- paste0("  WLE mean=",
                        round(WLEM,digits=digits), "\n")
        cat(v1)
        v1 <- paste0("  WLE variance=",
                        round(WLEvar,digits=digits), "\n")
        cat(v1)
    }
    if (ndim>1){
        for (dd in 1:ndim){
            cat("\n")
            v1 <- paste0("  WLE Reliability (Dimension ", dd,
                        ")=", round(WLE.rel[dd],digits=digits), "\n")
            cat(v1)
            v1 <- paste0("  Average error variance (Dimension ", dd,
                        ")=", round(M_sq_error[dd],digits=digits), "\n")
            cat(v1)
            v1 <- paste0("  WLE mean (Dimension ", dd,
                        ")=", round(WLEM[dd],digits=digits), "\n")
            cat(v1)
            v1 <- paste0("  WLE variance (Dimension ", dd,
                        ")=", round(WLEvar[dd],digits=digits), "\n")
            cat(v1)
        }
    }
}
#**********************************************************************
