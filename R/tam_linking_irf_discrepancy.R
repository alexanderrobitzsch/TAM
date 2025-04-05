## File Name: tam_linking_irf_discrepancy.R
## File Version: 0.061

tam_linking_irf_discrepancy <- function(probs1, probs2, wgt, type,
    pow_rob_hae=1, eps_rob_hae=1e-4)
{
    K <- min( dim(probs1)[3], dim(probs2)[3])
    crit <- 0
    #-- define Haebara criterion function
    if (type %in% c('Hae','RobHae') ){
        for (kk in 1L:K){
            irf_diff <- probs1[,,kk,drop=FALSE] - probs2[,,kk,drop=FALSE]
            irf_loss <- tam_linking_function_haebara_loss(x=irf_diff, type=type,
                            pow_rob_hae=pow_rob_hae, eps=eps_rob_hae)
            crit <- crit + sum( irf_loss * wgt )
        }
    }
    #-- define Stocking-Lord criterion function
    if (type=='SL'){
        vcrit <- 0
        for (kk in 1L:K){
            vcrit <- vcrit + (kk-1)*( probs1[,,kk,drop=FALSE] - probs2[,,kk,drop=FALSE] )
        }
        vcrit <- rowSums( vcrit )
        crit <- sum( vcrit^2 * wgt )
    }
    return(crit)
}
