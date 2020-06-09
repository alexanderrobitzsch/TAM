## File Name: tam_jml_itempartable.R
## File Version: 0.02


tam_jml_itempartable <- function(resp, maxK, AXsi, B, resp.ind)
{
    ndim <- dim(B)[3]
    item <- tam_itempartable( resp=resp, maxK=maxK, AXsi=AXsi,
                    B=B, ndim=ndim, resp.ind=resp.ind, rprobs=NULL,
                    n.ik=NULL, pi.k=NULL, order=FALSE )
    return(item)
}
