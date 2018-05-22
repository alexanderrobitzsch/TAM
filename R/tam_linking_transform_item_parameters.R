## File Name: tam_linking_transform_item_parameters.R
## File Version: 0.06

tam_linking_transform_item_parameters <- function( B, AXsi, A, trafo_items )
{
    B_trans <- B
    AXsi_trans <- AXsi
    K <- ncol(AXsi)
    for (kk in 2:K){
        B_trans[,kk,] <- B[,kk,] * trafo_items["a"]
        AXsi_trans[,kk] <- B[,kk,] * trafo_items["b"] + AXsi[,kk]
    }
    #--- determine transformed xsi parameter
    xsi_trans <- tam_AXsi_fit( AXsi=AXsi_trans, A=A )
    #--- OUTPUT
    res <- list( B=B_trans, AXsi=AXsi_trans, xsi=xsi_trans )
    return(res)
}
