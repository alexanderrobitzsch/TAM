## File Name: tam_mml_3pl_calc_total_ll.R
## File Version: 0.210

tam_mml_3pl_calc_total_ll <- function( iIndex, A, B, xsi, theta,
            nnodes, guess, n.ik, eps, return_probs_na=FALSE, probs_na=NULL )
{
    AXsi <- tam_mml_compute_AXsi(A=A, xsi=xsi)
    maxK <- dim(A)[2]
    probs0 <- tam_mml_3pl_calc_prob( iIndex=iIndex, A=A, AXsi=AXsi, B=B,
                        xsi=xsi, theta=theta, nnodes=nnodes, maxK=maxK,
                        guess=guess )$rprobs
    if (is.null(probs_na)){
        probs_na <- is.na(probs0)
    }
    if (sum(probs_na)>0){
        probs0[ probs_na ] <- 0
    }
    n.ik <- n.ik[ iIndex,,, drop=FALSE ]
    ll0 <- tam_mml_3pl_calc_ll( n.ik=n.ik, probs=probs0, eps=eps )
    if (return_probs_na){
        res <- list(ll0=ll0, probs_na=probs_na)
    } else {
        res <- ll0
    }
    return(res)
}

