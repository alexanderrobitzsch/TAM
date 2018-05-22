## File Name: tam_round_data_frame.R
## File Version: 0.04

tam_round_data_frame <- function(obji, from=1, to=ncol(obji), digits=3,
    rownames_null=FALSE)
{
    if ( is.matrix(obji) | is.data.frame(obji) ){
        for ( vv in from:to ){
            obji[,vv] <- round( obji[,vv], digits )
        }
        if (rownames_null){
            rownames(obji) <- NULL
        }
    }
    if ( is.vector(obji) ){
        obji <- round(obji, digits)
    }
    return(obji)
}
