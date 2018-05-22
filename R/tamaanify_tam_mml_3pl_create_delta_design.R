## File Name: tamaanify_tam_mml_3pl_create_delta_design.R
## File Version: 9.07

###################################################
# create delta design matrix
tamaanify_tam_mml_3pl_create_delta_design <- function(X, main=2, int=TRUE ){
    D <- ncol(X)
    theta <- X
    deltaD <- matrix( 1, nrow=nrow(theta), ncol=1)
    for (dd in 1:D){
        for (mm in 1:main){
            deltaD <- cbind( deltaD, theta[,dd]^mm )
        }
    }
    if ( D>1){
        if (int ){
            for (dd1 in 1:(D-1) ){
                for (dd2 in (dd1+1):D){
                    deltaD <- cbind( deltaD, theta[,dd1]*theta[,dd2] )
                }
            }
        }
    }
    return(deltaD)
}
############################################################

create.deltaDesign <- tamaanify_tam_mml_3pl_create_delta_design
