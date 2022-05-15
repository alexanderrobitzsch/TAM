## File Name: tamaanify_tam_mml_3pl_designMatrices.R
## File Version: 9.215

### design matrices for estimation with tam.mml.3pl method

tamaanify_tam_mml_3pl_designMatrices <- function(res)
{
    res <- switch( res$ANALYSIS.list$type,
            "LCA"=tamaanify.tam.mml.3pl.designMatrices.LCA(res=res),
            "LOCLCA"=tamaanify.tam.mml.3pl.designMatrices.LOCLCA(res=res),
            "OLCA"=tamaanify.tam.mml.3pl.designMatrices.OLCA(res=res),
            "TRAIT"=tamaanify.tam.mml.3pl.designMatrices.TRAIT(res=res),
            "MIXTURE"=tamaanify.tam.mml.3pl.designMatrices.MIXTURE(res=res)
            )
    # output
    return(res)
}
