## File Name: tam_mml_mfr_proc_compute_maxKi.R
## File Version: 0.03

tam_mml_mfr_proc_compute_maxKi <- function(resp, facets)
{
    if( ncol(resp)>1 ){
        maxKi <- apply( resp , 2 , max , na.rm=TRUE )
    } else {
        if(ncol(resp)==1){
            item.ind <- grep("Item", names(facets), ignore.case=TRUE)
            if( !is.null(item.ind) ){
                if ( length(item.ind) == 0 ){
                    item.ind <- NULL
                }
            }
            if( ! is.null(item.ind) ){
                maxKi <- stats::aggregate( resp , facets[,item.ind,drop=FALSE] ,
                                max, na.rm=TRUE )[,2]
            } else {
                maxKi <- stats::aggregate( resp , facets[,1,drop=FALSE] ,
                                max, na.rm=TRUE )[,2]
          }
        }
    }
    #--- OUTPUT
    res <- list(maxKi=maxKi)
    return(res)
}

