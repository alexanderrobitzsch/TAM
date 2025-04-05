## File Name: tam_linking_extract_parameters_trait_distribution.R
## File Version: 0.039

tam_linking_extract_parameters_trait_distribution <- function(tamobj)
{
    class_tamobj <- class(tamobj)
    G <- tamobj$G
    group <- tamobj$group
    M <- as.vector( unlist(tamobj$beta) )
    variance <- tamobj$variance
    if ( class_tamobj %in% c('tam.mml', 'tam.mml.2pl', 'tam.mml.mfr') ){
        if (G > 1){
            variance_group <- tam_aggregate( variance, group, mean=TRUE)
            SD <- sqrt( variance_group[,2] )
        } else {
            SD <- sqrt(variance[1,1])
        }
    } else {
        if (!is.null(variance)){
            SD <- sqrt( as.vector( unlist( variance ) ) )
        } else {
            SD <- 1
        }
    }
    if (is.null(G)){ G <- 1 }
    if (is.null(M)){ M <- 0 }

    #--- output
    res <- list( class_tamobj=class_tamobj, G=G, group=group, M=M, SD=SD)
    return(res)
}
