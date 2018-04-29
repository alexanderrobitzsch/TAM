## File Name: tamaanify.tam.mml.3pl.deltadesign.R
## File Version: 9.06

###################################
#*** delta design matrix
tamaanify.tam.mml.3pl.deltadesign <- function(res)
{
    anlist <- res$ANALYSIS.list
    delta.fixed <- NULL
    #*******************************************
    #*** delta design matrix mixture distribution
    if ( res$ANALYSIS.list$type == "MIXTURE" ){
        theta <- res$theta_MIXTURE
        TG <- nrow(theta)
        ncl <- anlist$NCLASSES
        D <- ncol(theta)
        TP <- TG*ncl
        delta.designmatrix <- matrix( 0 , nrow=TP , ncol=0)
        th1 <- tamaanify_tam_mml_3pl_create_delta_design(X=theta , main=2 , int=TRUE )
        # bind two matrices
        delta.designmatrix <- th1
        for (cl in 2:ncl){
            delta.designmatrix <- tam_rbind_twomatrices(X1=delta.designmatrix , X2=th1)
        }
        rownames(delta.designmatrix) <- dimnames(res$E)[[3]]
        res$delta.designmatrix <- delta.designmatrix
    }
    #******************************************
    res$delta.fixed <- delta.fixed
    return(res)
}
