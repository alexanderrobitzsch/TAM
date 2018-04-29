## File Name: summary_tamaan_3pl_distr_mixture.R
## File Version: 0.03



###################################################################
# distribution mixture
summary_tamaan_3pl_distr_mixture <- function(object)
{
    cat("------------------------------------------------------------\n")
    cat("Class Probabilities\n")
    obji <- round( object$probs_MIXTURE , 3 )
    print( obji )
    cat("------------------------------------------------------------\n")
    cat("Class Distributions\n\n")
    mom <- object$moments_MIXTURE
    ncl <- length(mom)
    for (cl in 1:ncl){
        cat("******\nClass" , cl , "\n\n")
        mom.cl <- mom[[cl]]
        mom.cl$skewness.trait <- NULL
        for (nn in names(mom.cl)[1:2]){
            mom.cl[[nn]] <- round( mom.cl[[nn]] , 3 )
        }
        for (nn in names(mom.cl[[3]]) ){
            mom.cl[[3]][[nn]] <- round( mom.cl[[3]][[nn]] , 3 )
        }
        print(mom.cl)
    }
}
#################################################################
