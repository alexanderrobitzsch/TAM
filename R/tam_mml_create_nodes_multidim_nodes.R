## File Name: tam_mml_create_nodes_multidim_nodes.R
## File Version: 0.05

tam_mml_create_nodes_multidim_nodes <- function(nodes, ndim)
{
    use_expand_grid <- TRUE
    if (ndim==1){
        theta <- matrix( nodes, ncol=1)
        use_expand_grid <- FALSE
    }
    if (ndim > 1){
        if ( is.matrix(nodes) ){
            theta <- nodes
            use_expand_grid <- FALSE
        }
    }
    if (use_expand_grid){
        theta <- as.matrix( expand.grid( as.data.frame( matrix( rep(nodes, ndim), ncol=ndim ))))
    }
    return(theta)
}
